package com.gdn.mta.product.repository;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.pdt.feign.PDTFeign;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

@Repository
public class ProductDistributionTaskRepositoryBean implements ProductDistributionTaskRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductDistributionTaskRepositoryBean.class);

  @Autowired
  private PDTFeign pdtFeign;

  @Value("${pdt.product.retry.enabled}")
  private boolean addProductToPdtRetry;

  @Override
  public void moveFailedProductToQC(String requestId, String username, String productCode) throws Exception {
    GdnBaseRestResponse response = pdtFeign.moveFailedProductToQC(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,requestId, username, productCode);
    if (!response.isSuccess()) {
      throw new IllegalStateException("PDT moveFailedProductToQC failed : " + response.getErrorMessage());
    }
  }

  @Override
  public DistributionProductDetailResponse getDetailsForAnyProduct(String requestId, String username,
      String productCode) throws Exception {
    GdnRestSingleResponse<DistributionProductDetailResponse> response =
        pdtFeign.getDetailsForProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID,requestId, username, productCode);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void removeProductFromPDT(String requestId, String username, RemoveProductRequest request) throws Exception {
    GdnBaseRestResponse response = pdtFeign.removeProductFromPDT(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,requestId, username, request);
    if (!response.isSuccess() && addProductToPdtRetry) {
      LOGGER.error("Removal of Product from PDT failed due to Internal Error for productCode : {} . errorMessage: {}",
          request.getProductCode(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    } else {
      LOGGER.info("Product-workflow-tracker : product successfully removed from PDT productCode : {}",
          request.getProductCode());
    }
  }

  @Override
  public PDTProductDomainEventModel getPDTDomainModelResponseByCode(String requestId, String username,
      String productCode) throws Exception {
    GdnRestSingleResponse<PDTProductDomainEventModelResponse> response =
        pdtFeign.getPDTDomainModelResponseByCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID,requestId, username, productCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT getPDTDomainModelResponseByCode failed : {} : {}", productCode, response.getErrorMessage());
      throw new IllegalStateException("PDT getPDTDomainModelResponseByCode failed");
    }
    return response.getValue().getPdtProductDomainEventModel();
  }

  @Override
  public boolean checkIfProductExistsInPDT(String productCode, boolean allProducts) {
    GdnRestSimpleResponse<Boolean> response = pdtFeign
        .checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, productCode,
            allProducts);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT checkIfProductExistsInPDT failed : {} ", productCode);
      throw new IllegalStateException("PDT checkIfProductExistsInPDT failed");
    }
    return response.getValue();
  }

  @Override
  public ProductImageQcFeedbackResponse getProductImageQcFeedback(String storeId,
      String productCode) {
    GdnRestSimpleResponse<ProductImageQcFeedbackResponse> response = pdtFeign
        .getProductImageFeedback(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, productCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT getProductImageQcFeedback failed : {} ", productCode);
      return null;
    }
    return response.getValue();
  }

  @Override
  public void sendProductBackToVendor(String storeId, String productCode) {
    GdnBaseRestResponse response = pdtFeign
        .sendProductBackToVendor(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, productCode);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT failed to add back to vendor : {} ", productCode);
    }
  }

  @Override
  public void sendProductToAutoNeedRevision(AutoNeedRevisionRequest autoNeedRevisionRequest,
      boolean validateAssignment) {
    GdnBaseRestResponse response = pdtFeign
        .sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, validateAssignment,
            autoNeedRevisionRequest);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT failed to sendProductToAutoNeedRevision productCode: {} ", autoNeedRevisionRequest.getProductCode());
      throw new IllegalStateException("PDT sendProductToAutoNeedRevision failed");
    }
  }

  @Override
  public void productRetryStatusUpdate(String productCode, ProductRetryStatusUpdate productRetryStatusUpdate) {
    GdnBaseRestResponse response = pdtFeign
        .productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
            productCode, productRetryStatusUpdate);
    if (Objects.isNull(response) || !response.isSuccess()) {
      LOGGER.error("PDT failed to updateStateInPDT productCode: {} ", productCode);
      throw new IllegalStateException("PDT updateStateInPDT failed");
    }
  }

  @Override
  public void updateProductBrand(ChangeBrandRequest changeBrandRequest) throws Exception {
    GdnBaseRestResponse response = pdtFeign.updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest);
    if (Objects.isNull(response)) {
      LOGGER.error("PDT failed to update brand with changeBrandRequest = {} ", changeBrandRequest);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, ErrorCategory.UNSPECIFIED.getMessage());
    }
    if (!response.isSuccess()) {
      LOGGER.error("PDT failed to update brand with changeBrandRequest = {} ", changeBrandRequest);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public AppealProductResponse updateAppealProduct(AppealProductRequest appealProductRequest)
    throws ApplicationException {
    GdnRestSingleResponse<AppealProductResponse> response =
      pdtFeign.updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
        appealProductRequest);
    if (Objects.isNull(response)) {
      LOGGER.error("PDT failed to update appeal product with request = {} ", appealProductRequest);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
        ErrorCategory.UNSPECIFIED.getMessage());
    }
    if (!response.isSuccess()) {
      LOGGER.error("PDT failed to update appeal product with request = {} ", appealProductRequest);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }
}
