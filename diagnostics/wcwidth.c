/* How many columns a character takes on a terminal.

   The library that draws errors asks the C library for this, and the C
   library of Windows has no such function. This is one for there, and it
   is only built there. It knows what the drawing needs: nothing for what
   is joined to the character before it, two for the wide characters of
   East Asia, and one for the rest. A character does not fit in more than
   sixteen bits there, so nothing above that is asked about. */
int wcwidth(unsigned int character) {
  unsigned int c = character & 0xFFFF;
  if (c == 0) return 0;
  if (c < 0x20 || (c >= 0x7F && c < 0xA0)) return -1;
  if ((c >= 0x0300 && c <= 0x036F) || (c >= 0x200B && c <= 0x200F) || (c >= 0x20D0 && c <= 0x20FF)
      || (c >= 0xFE00 && c <= 0xFE0F) || (c >= 0xFE20 && c <= 0xFE2F))
    return 0;
  if ((c >= 0x1100 && c <= 0x115F) || (c >= 0x2E80 && c <= 0xA4CF) || (c >= 0xAC00 && c <= 0xD7A3)
      || (c >= 0xF900 && c <= 0xFAFF) || (c >= 0xFE30 && c <= 0xFE6F) || (c >= 0xFF00 && c <= 0xFF60)
      || (c >= 0xFFE0 && c <= 0xFFE6))
    return 2;
  return 1;
}
