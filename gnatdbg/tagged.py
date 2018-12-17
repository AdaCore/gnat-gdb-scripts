"""
Helpers to deal with values of tagged types.
"""

import gdb

from gnatdbg.utils import (
    address_as_offset, addr_to_val, encode_name, get_system_address,
    strip_typedefs
)

# The reader of this module is assumed to be familiar with how GNAT implements
# tagged records. See a-tags.ads in GNAT's sources to learn about it.


SIGNATURE_UNKNOWN = 'signature_unknown'
SIGNATURE_PRIMARY = 'signature_primary'
SIGNATURE_SECONDARY = 'signature_secondary'

SIGNATURES = (SIGNATURE_UNKNOWN, SIGNATURE_PRIMARY, SIGNATURE_SECONDARY)


_tsd_type_missing = False


def get_tsd_type():
    global _tsd_type_missing
    try:
        return gdb.lookup_type('ada__tags__type_specific_data')
    except gdb.error: # no-code-coverage
        if _tsd_type_missing:
            print('WARNING: the GNAT runtime is missing some debug'
                  ' information.  As a result, some debugging features related'
                  ' to tagged type will not work.')
        _tsd_type_missing = True
        raise


def reinterpret_tagged(tagged_value):
    """
    Reinterpret a tagged value according to its dynamic type.

    For instance, assume Child_Type derives from the Root_Type tagged type.
    This function will turn a Child_Type object with a Root_Type type into a
    true Child_Type value.

    :param gdb.Value tagged_value: Tagged value to re-interpret.
    :rtype: gdb.Value
    """
    tagged_value = auto_deref_tagged(tagged_value)
    primary_tag_addr, offset_to_top = get_dyntype_info(tagged_value)

    dyn_type_name = encode_name(tag_expanded_name(get_tsd(primary_tag_addr)))
    dyn_type = gdb.lookup_type(dyn_type_name)

    return addr_to_val(
        tagged_value.address.cast(get_system_address()) - abs(offset_to_top),
        dyn_type
    )


def tagged_field(tagged_value, field_name, reinterpret=True):
    """
    Look for a field in a tagged record's own and all its inherited fields.

    This is useful because we see tagged records in GDB as a record that
    contains another record (for fields from the parent type) that contains
    another record, etc.

    :param gdb.Value tagged_value: Can be either a tagged record or an access
        to a tagged record.
    :param str field_name: Name of the field to look up.
    :param bool reinterpret: If True, re-interpret `tagged_value` according to
        its dynamic type.
    :rtype: gdb.Value
    """
    tagged_value = (reinterpret_tagged(tagged_value)
                    if reinterpret else auto_deref_tagged(tagged_value))

    if tagged_value.type.code != gdb.TYPE_CODE_STRUCT:
        raise TypeError('Input type is not tagged')

    while True:
        try:
            return tagged_value[field_name]
        except gdb.error:
            pass
        try:
            tagged_value = tagged_value['_parent']
        except gdb.error:
            raise gdb.error('There is no member {}'.format(field_name))


def auto_deref_tagged(tagged_value):
    """
    Dereference access and strip typedefs layers around a tagged record.

    :param gdb.Value tagged_value: GDB value whose type can be either a
        (typedef'd) tagged type or a (typedef'd) access to a tagged type.
    :rtype: gdb.Value
    """
    tagged_value = strip_typedefs(tagged_value)
    while tagged_value.type.code in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_REF):
        tagged_value = strip_typedefs(tagged_value.referenced_value())
    return tagged_value


def decode_tag(tag_addr):
    """
    Extract some information from a tag.

    :param gdb.Value tag_addr: Pointer/access to the tag to process.
    :return (str, int, gdb.Value): A tuple that contains:
      1. The signature kind for this tag.
      2. The "offset to top" information.
      3. The address of the type specific data (TSD).
    """
    system_address = get_system_address()
    tag_addr = tag_addr.cast(system_address)

    addr_size = system_address.sizeof
    signature_kind = gdb.lookup_type('character')

    signature_addr = tag_addr - 3 * addr_size - 4
    offset_to_top_addr = tag_addr - 2 * addr_size
    tsd_addr = tag_addr - addr_size

    sig_int = int(addr_to_val(signature_addr, signature_kind))
    try:
        sig = SIGNATURES[sig_int]
    except IndexError:
        sig = '<invalid signature: {}>'.format(sig_int)

    offset_to_top = address_as_offset(addr_to_val(offset_to_top_addr,
                                                  system_address))

    return (
        sig,
        offset_to_top,
        addr_to_val(tsd_addr, system_address),
    )


def get_tag_addr(tagged_value):
    """
    Return the address of the tag corresponding to the given tagged value.

    :param gdb.Value tagged_value: Value for the tagged record to process.
    :rtype: gdb.Value
    """
    return tagged_field(tagged_value, '_tag', reinterpret=False)


def get_dyntype_info(tagged_value):
    """
    Compute information about the dynamic type of a tagged value.

    :param gdb.Value tagged_value: Value for the tagged record to process.
    :return (gdb.Value, int): A tuple that contains:
      1. The address of the primary dispatch table.
      2. The "offset to top" information for the input value.
    """
    system_address = get_system_address()
    tag_addr = get_tag_addr(tagged_value)
    signature, offset_to_top, _ = decode_tag(tag_addr)

    if signature == SIGNATURE_SECONDARY:
        record_addr = (tagged_value.address.cast(system_address) -
                       abs(offset_to_top))
        tag_addr = get_tag_addr(record_addr.cast(tagged_value.type.pointer()))
        signature, _, _ = decode_tag(tag_addr)

    if signature != SIGNATURE_PRIMARY:
        raise gdb.MemoryError('Corrupted tag')

    return (tag_addr.cast(system_address), offset_to_top)


def get_tsd(tag_addr):
    """
    Get the type specific data corresponding to a tag.

    This assumes that the tag is a primary dispatch table.

    :param gdb.Value tag_addr: Value that is the address of the tag to process.
    :rtype: gdb.Value
    """
    system_address = get_system_address()
    tsd_addr = addr_to_val(tag_addr - system_address.sizeof, system_address)
    return addr_to_val(tsd_addr, get_tsd_type())


def tag_expanded_name(tsd):
    """
    Get the expanded name corresponding to a tag.

    :param gdb.Value tsd: Result of the `get_tsd` function.
    :rtype: str
    """
    char_ptr = gdb.lookup_type('character').pointer()
    name = tsd['expanded_name'].cast(char_ptr)
    return name.string()
