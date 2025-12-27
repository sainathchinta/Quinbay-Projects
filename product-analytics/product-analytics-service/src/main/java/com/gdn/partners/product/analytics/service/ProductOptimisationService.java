package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.ProductOptimisationListResponse;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationSuggestionResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.product.analytics.web.model.ProductCountsWebResponse;

import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationUpdateStatusRequest;
import model.ProductChangeEventModel;
import model.ProductOptimisationEventModel;
import org.springframework.data.domain.Page;

import java.util.List;


public interface ProductOptimisationService {

  /**
   *
   * @param storeId storeId
   * @param sellerCode sellerCode
   * @return product count
   */
  ProductCountsWebResponse getProductCounts(String storeId, String sellerCode);

  /**
   * delete products which are archived, suspended, rejected or taken down.
   *
   * @param productChangeEventModel
   */
  void removeDeletedProduct(ProductChangeEventModel productChangeEventModel);

  /**
   * update status for product optimise and feedback collection
   *
   * @param productOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest
   */
  void updateStatusForProductOptimisation(
      ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest);

  /**
   * fetch list of products to optimise for a seller
   *
   * @param productOptimisationListRequest
   * @return
   */
  Page<ProductOptimisationListResponse> fetchProductOptimisationList(String storeId,
      ProductOptimisationListRequest productOptimisationListRequest, int page, int size);

  /**
   * to clear seller level cache
   *
   * @param sellerCode
   */
  void clearSellerLevelCache(String sellerCode);

  /**
   * provide Feedback
   *
   * @param productOptimisationFeedbackRequest productOptimisationFeedbackRequest
   */
  void submitSuggestionFeedback(ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest);

  /**
   * Upsert product optimisation data in db
   * @param productOptimisationEventModel Product Optimisation Event Model
   */
  void upsertProductOptimisationData(ProductOptimisationEventModel productOptimisationEventModel);

  /**
   *
   * @param storeId storeId
   * @param productSku productSku
   * @return list of productOptimisationSuggestionResponse
   */
  List<ProductOptimisationSuggestionResponse> showSuggestionDetails(String storeId, String productSku);
}