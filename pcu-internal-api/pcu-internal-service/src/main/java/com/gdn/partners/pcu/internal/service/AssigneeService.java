package com.gdn.partners.pcu.internal.service;

import java.util.List;

import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;

public interface AssigneeService {

  /**
   * Get assignee's list by filter request, activated and viewable flag
   *
   * @param request
   * @param activated
   * @param viewable
   * @return
   */
  List<String> getAssigneesByFilterRequestAndActivatedAndViewableFlag(ReviewProductsFilterRequest request, boolean activated,
      boolean viewable);
}
