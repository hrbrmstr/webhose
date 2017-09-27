c("relevancy", "social.facebook.likes", "social.facebook.shares", "social.facebook.comments",
  "social.gplus.shares", "social.pinterest.shares", "social.linkedin.shares",
  "social.stumbledupon.shares", "social.vk.shares", "replies_count", "participants_count",
  "spam_score", "performance_score", "published", "thread.published", "domain_rank",
  "ord_in_thread", "rating") -> sort_params

comma <- function (x, ...) {
  format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}