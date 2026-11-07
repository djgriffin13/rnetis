# COMPOSITE KEYS, AND THE ONE SEPARATOR THEY ARE BUILT FROM.
#
# Several layers key a row by more than one column: (group, actor) for a
# person, (group, wave) for an occasion, and the schema's arbitrary key
# columns. All of them paste, and all of them must paste the SAME way.
#
# WHY THE SEPARATOR IS DEFINED ONCE. A literal repeated at a dozen call sites
# is a convention that drifts, and two parts of one package keying the same
# pair differently is how they stop agreeing about who someone is.
#
# WHY IT IS A CONTROL CHARACTER. Group and actor labels are user data and
# contain punctuation, so any printable separator is forgeable: with "::", the
# group "a" with actor "b::c" and the group "a::b" with actor "c" produce the
# same key. That does not error -- it POOLS, so anything splitting by the key
# reports a correctly computed statistic for a unit that does not exist. A
# carriage return is not plausible in a label, and `.netis_unit_key()` refuses
# one that contains it, so the guarantee is enforced rather than argued.
#
# WHY (group, actor) HAS ITS OWN HELPER. Groups have disjoint actor sets, and
# the same label in two groups is two different people, so the PAIR is the
# identity everywhere -- actor alone is never a key. The helper makes that hard
# to forget at a call site.

#' @noRd
.NETIS_KEY_SEP <- "\r"

#' The composite (group, actor) key
#'
#' @param group,actor equal-length vectors.
#' @return the composite keys, as character.
#' @noRd
.netis_unit_key <- function(group, actor) {
  group <- as.character(group)
  actor <- as.character(actor)
  # NO SEPARATOR IS COLLISION-PROOF IF A LABEL MAY CONTAIN IT, so the
  # guarantee is enforced rather than argued. Choosing a character unlikely
  # to appear in typed data makes a collision RARE; refusing a label that
  # contains it makes the key injective, which is what every caller here
  # already assumes.
  #
  # It matters because a collision does not fail loudly. Two units merge into
  # one key, and anything that splits by that key -- a per-actor summary, say
  # -- then reports a correctly computed statistic for a unit that does not
  # exist. A printable separator makes that reachable from ordinary data:
  # with "::", the group "a" with actor "b::c" and the group "a::b" with actor
  # "c" key identically.
  bad <- grepl(.NETIS_KEY_SEP, group, fixed = TRUE) |
    grepl(.NETIS_KEY_SEP, actor, fixed = TRUE)
  if (any(bad)) {
    i <- which(bad)[1]
    stop(sprintf(
      "A group or actor label contains the carriage return this package uses to join them into a unit key (group %s, actor %s). Two different units could then produce the same key and be silently merged, so the label is refused rather than encoded around. Remove the control character from the identifier.",
      encodeString(group[i], quote = "\""), encodeString(actor[i], quote = "\"")),
      call. = FALSE)
  }
  paste(group, actor, sep = .NETIS_KEY_SEP)
}
