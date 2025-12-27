package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.ProductAssigneeChangeResponse;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import org.springframework.data.domain.Page;

import java.util.List;

public interface AutoApprovedService {

  /**
   * to fetch list of auto approved list
   *
   * @param request
   * @param page
   * @param size
   * @return AutoApprovedListWebResponse Page
   */
  Page<AutoApprovedListWebResponse> fetchListOfAutoApprovedProducts(AutoApprovedWebRequest request,
    int page, int size);

  /**
   * to fetch selected items of auto approved products
   *
   * @param request
   * @return list of AutoApprovedListWebResponse
   */
  List<AutoApprovedListWebResponse> fetchSelectedItemsOfAutoApprovedProducts(
    AutoApprovedSelectedDownloadRequest request);

  /**
   * Update assignee of a non null product code
   *
   * @param approvedAssigneeRequest approvedAssigneeRequest
   * @return list of product code with error message
   */
  List<ProductAssigneeChangeResponse> updateAssignee(
    AutoApprovedAssigneeRequest approvedAssigneeRequest);

  /**
   * Delete auto approved product
   * @param productCode product code to delete
   */
  void deleteAutoApprovedProduct(String productCode, String action);
}
