package ph.samson.atbp.plate

object CookingExclude {

  /** When a non-empty exclude list is passed to `plate check`, only cooking may
    * use it. Returns an error message naming the status, or None when the
    * combination is allowed.
    */
  def rejectedStatus(
      status: String,
      excludeProjects: List[String]
  ): Option[String] =
    if (excludeProjects.isEmpty || status == "cooking") None
    else Some(s"--exclude is not supported for plate check --status $status")
}
