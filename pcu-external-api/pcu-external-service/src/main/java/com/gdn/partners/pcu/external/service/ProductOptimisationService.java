package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationSuggestionResponse;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ProductOptimisationService {

  /**
   *
   * @param storeId storeId
   * @param requestId requestId
   * @param sellerCode sellerCode
   * @return product count
   */
  ProductCountsWebResponse getProductCount(String storeId, String requestId, String sellerCode);

  /**
   * fetch list of products to optimise with filter applied
   *
   * @param sellerCode
   * @param productOptimisationListWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<ProductOptimisationListResponse> fetchProductOptimisationList(String sellerCode,
      ProductOptimisationListWebRequest productOptimisationListWebRequest, int page, int size);

  /**
   *
   * @param productOptimisationFeedbackRequest productOptimisationFeedbackRequest
   */
  void submitSuggestionFeedback(ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest);

  /**
   *
   * @param productSku
   * @return
   */
  List<ProductOptimisationSuggestionResponse> getSuggestionDetails(String productSku);

  /**
   * update status for product optimise and feedback collection
   *
   * @param requestId requestId
   * @param productOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest
   */
  void updateStatusForProductOptimisation(String requestId, ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest);
}
