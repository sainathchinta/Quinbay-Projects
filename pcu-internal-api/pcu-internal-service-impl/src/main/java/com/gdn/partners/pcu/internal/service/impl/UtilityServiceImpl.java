package com.gdn.partners.pcu.internal.service.impl;

import java.util.Collections;
import java.util.List;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.service.UtilityService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovalEligibilityWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemViewConfigWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PDTStateUpdateWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UtilityServiceImpl implements UtilityService {

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private XBulkFeign xBulkFeign;

  @Autowired
  private MasterCatalogFeign masterCatalogFeign;
  @Qualifier("com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign")
  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public void publishAddEditedEventToPDT(String productCode, String reviewType) {
    GdnBaseRestResponse response = pbpFeign.publishEditedEvent(productCode, reviewType);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void publishAddRevisedEventToPDT(String productCode) {
    GdnBaseRestResponse response = pbpFeign.publishRevisedEvent(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void retryProductImageResize(String productCode) {
    GdnBaseRestResponse response = pbpFeign.retryResizeImage(productCode, false);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void retryEditedProductImageResize(String productCode) {
    GdnBaseRestResponse response = pbpFeign.retryEditedResizeImage(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePBPProductWorkflow(String productCode, String state) {
    GdnBaseRestResponse response = pbpFeign.updateProductWorkflow(productCode, state);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePBPReviewPending(String productCode, boolean reviewPending) {
    GdnBaseRestResponse response = pbpFeign.updateReviewPending(productCode, reviewPending);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePBPActivatedAndViewable(String productCode, boolean activated, boolean viewable) {
    GdnBaseRestResponse response = pbpFeign.updateActivatedAndViewable(productCode, activated, viewable);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public String checkProductAutoApprovalEligibility(String productCode,
      AutoApprovalEligibilityWebRequest autoApprovalEligibilityWebRequest) {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response = pbpFeign.checkAutoApprovalEligibility(productCode,
        RequestHelper.toAutoApprovalTypeRequest(autoApprovalEligibilityWebRequest));
    ResponseHelper.validateResponse(response);
    return response.getValue().getAutoApprovalType();
  }

  @Override
  public void republishPCBProductPublishEvent(String productCode, String operationType) {
    GdnBaseRestResponse response =
        pcbFeign.republishProductFromPCB(Collections.singletonList(productCode), operationType);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void clearPCBProductCache(String productCode, String productId) {
    GdnBaseRestResponse response = pcbFeign.clearPCBCache(productId, productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePCBProductViewable(String productCode, boolean viewable) {
    GdnBaseRestResponse response = pcbFeign.updateViewable(productCode, viewable);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePCBProductReviewPending(String productCode, boolean reviewPending) {
    GdnBaseRestResponse response = pcbFeign.updateReviewPending(productCode, reviewPending);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updatePDTState(String productCode, PDTStateUpdateWebRequest pdtStateUpdateWebRequest) {
    GdnBaseRestResponse response = pdtFeign
        .updateProductStateInPDT(productCode, RequestHelper.toProductRetryStatusUpdate(pdtStateUpdateWebRequest));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void takeDownOrReactivateProduct(String productSku, boolean forceReview,
      List<ItemViewConfigWebRequest> request) {
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests =
        RequestHelper.toItemViewConfigAndItemSkuRequest(request);
    GdnBaseRestResponse response =
        xProductFeign.takeDownOrReactivateProduct(forceReview, itemViewConfigAndItemSkuRequests);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void abortPendingBulkProcessById(String id) {
    GdnBaseRestResponse response = xBulkFeign.abortPendingBulkProcess(id);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void abortPendingDownloadsByEntity(String entity, String status) {
    GdnBaseRestResponse response = xBulkFeign.clearInProgressDownloads(entity, status);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void deleteProductCollection(String userName, String requestId, String storeId,
    String clientId, String channelId, boolean needEmailNotification,
    DeleteProductWebRequest deleteProductWebRequest) {
    GdnBaseRestResponse response =
      pbpFeign.deleteProductCollection(storeId, channelId, clientId, requestId, userName,
        needEmailNotification,
        RequestHelper.toDeleteProductRequest(userName, deleteProductWebRequest));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void reindexActiveProductByProductCode(String productCode) {
    GdnBaseRestResponse response = pbpFeign.reindexActiveProductByProductCode(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void fetchDataFromBigQueryForMasterSkuReview(String processName,
      BigQueryFetchWebRequest request) {
    GdnBaseRestResponse response = masterCatalogFeign.fetchDataFromBigQuery(processName,
        RequestHelper.toBigQueryFetchRequest(request));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void migrateProductAndL5DetailsByProductSku(String storeId, String requestId,
    String clientId, String channelId, ProductAndL5MigrationRequest productAndL5MigrationRequest,
    String username) {
    GdnBaseRestResponse response =
      pbpFeign.migrateProductAndL5DetailsByProductSku(storeId, channelId, clientId, requestId,
        username, productAndL5MigrationRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updateSystemParameterInPCB(String storeId, String requestId, String channelId,
    String clientId, SystemParameterRequest systemParameterRequest) {
    pcbFeign.updateSystemParameter(storeId, requestId, channelId, clientId, systemParameterRequest);
  }

  @Override
  public void generateProductScoreByProductSkuOrProductCode(String storeId, String channelId,
    String clientId, String requestId, String username, boolean updateCategory, String productSku,
    String productCode) {
    GdnBaseRestResponse response =
      xProductFeign.generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId,
        requestId, username, updateCategory, productSku, productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void publishProductAttributeExtractions(String storeId,
      ProductAttributeExtractionsRequest productAttributeExtractionsRequest) {
    GdnBaseRestResponse response =
        productAnalyticsFeign.publishEventsByProductSku(storeId, productAttributeExtractionsRequest);
    ResponseHelper.validateResponse(response);
  }
}
