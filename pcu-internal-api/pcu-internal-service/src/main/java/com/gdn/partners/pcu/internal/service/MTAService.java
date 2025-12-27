package com.gdn.partners.pcu.internal.service;

import java.util.List;
import java.util.Map;

public interface MTAService {

  /**
   * Get image or content reviewers
   *
   * @param reviewerType
   * @return
   */
  Map<String, List<String>> getImageOrContentReviewers(String reviewerType);


}
