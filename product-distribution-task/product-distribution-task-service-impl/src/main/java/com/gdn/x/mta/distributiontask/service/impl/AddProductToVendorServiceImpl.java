package com.gdn.x.mta.distributiontask.service.impl;

import java.io.IOException;
import java.util.Collections;
import java.util.Objects;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.modal.AutoApprovalTypeRequestModel;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.config.BeanConfiguration;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.EditedReviewTypeConstants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.service.api.AddProductToVendorService;
import com.gdn.x.mta.distributiontask.service.api.EditedProductService;
import com.gdn.x.mta.distributiontask.service.api.ErrorMessages;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.RevisedProductService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AddProductToVendorServiceImpl implements AddProductToVendorService {
  private static final String STORE_ID = "10001";

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Autowired
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private EditedProductService editedProductService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private RevisedProductService revisedProductService;

  @Autowired
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Override
  public void processScreeningApprovalEvent(ScreeningProductApprovalEvent screeningProductApprovalEvent,
      int prioritySeller) throws Exception {
    ProductDetailResponse productDetailResponse =
        distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
            screeningProductApprovalEvent.getProductCode());
    productDetailResponse.setUpdatedBy(screeningProductApprovalEvent.getUpdatedBy());
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse = null;
    imageQcProcessedAndBrandResponse =
        distributionTaskService.getImageQcResponseByProductCode(productDetailResponse.getProductCode());
    Product product =
        ProductDomainEventModelConverterUtils.convertProductDomainEventModelToProduct(productDetailResponse,
            screeningProductApprovalEvent, false, imageQcProcessedAndBrandResponse);
    product.setPrioritySeller(prioritySeller);
    product = this.distributionTaskService.autoDistribute(product, productDetailResponse.isForReview());
    ProductReviewer productReviewer =
        ProductReviewer.builder().approvedDate(null).assignedDate(null).approverAssignee(null)
            .productCode(product.getProductCode()).build();
    productReviewer.setStoreId(product.getStoreId());
    productReviewer.setCreatedBy(product.getCreatedBy());
    solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(
        Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
    if (product.isPostLive()) {
      this.distributionTaskService.publishVendorApprovedEvent(product, true);
    }
    if ((Objects.nonNull(imageQcProcessedAndBrandResponse) && Objects.nonNull(
        imageQcProcessedAndBrandResponse.getImageQcProcessedResponse()))) {
      log.debug("Response for image qc from PBP : {}", imageQcProcessedAndBrandResponse);
      ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
      productImageQcFeedbackRequest.setProductCode(screeningProductApprovalEvent.getProductCode());
      productImageQcFeedbackRequest.setSystemFeedback(
          imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getImageQcResponse());
      productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    }
  }

  @Override
  public void processAddEditedProductEvent(AddEditedProductToPDTEvent addEditedProductToPDTEvent) throws Exception {
    if (addEditedProductToPDTEvent.isOnlyImageQcDataUpdate()) {
      productWrapperService.updateImageQcResponseByProductCode(
          addEditedProductToPDTEvent.getImageQcProcessedResponseDomainEvent());
      return;
    }
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse = null;
    imageQcProcessedAndBrandResponse =
        distributionTaskService.getImageQcResponseByProductCode(addEditedProductToPDTEvent.getProductCode());
    Product product =
        editedProductService.editProductDetails(addEditedProductToPDTEvent.getProductCode(), addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    product.setSellerType(addEditedProductToPDTEvent.isTrustedSeller() ?
      SellerType.TRUSTED_SELLER :
      SellerType.NON_TRUSTED_SELLER);
    product.setSellerBadge(
      SellerBadgeConstants.fromSellerBadgeConstants(addEditedProductToPDTEvent.getSellerBadge()));
    log.info("product data saved after edit : {}", product.getProductCode());
    ProductReviewer productReviewer =
        this.productReviewerService.findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(STORE_ID,
            product.getProductCode());
    solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(
        Collections.singletonList(new ProductAndReviewerDetailsDTO(product, productReviewer)));
    log.info("product data saved to solr after edit : {}", product.getProductCode());
    if ((Objects.nonNull(imageQcProcessedAndBrandResponse) && Objects.nonNull(
        imageQcProcessedAndBrandResponse.getImageQcProcessedResponse()))
        && (EditedReviewTypeConstants.IMAGE_EDIT.equals(addEditedProductToPDTEvent.getReviewTypes()))) {
      log.debug("Response for image qc from PBP : {}", imageQcProcessedAndBrandResponse);
      ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
      productImageQcFeedbackRequest.setProductCode(addEditedProductToPDTEvent.getProductCode());
      productImageQcFeedbackRequest.setSystemFeedback(
          imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getImageQcResponse());
      productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    }
    log.info("com.gdn.pbp.add.edited.product.to.pdt processing done for productCode : {}", product.getProductCode());
  }

  @Override
  public void processAddRevisedProductEvent(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(addRevisedProductToPDTEvent.getProductCode()), ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse =
        distributionTaskService.getImageQcResponseByProductCode(addRevisedProductToPDTEvent.getProductCode());
    ProductAndReviewerDetailsDTO productAndReviewerDetails = revisedProductService
        .addRevisedProductOnSubmit(addRevisedProductToPDTEvent, imageQcProcessedAndBrandResponse);
    log.info("product data saved after revision : {}", productAndReviewerDetails.getProduct().getProductCode());
    productAndReviewerDetails.getProduct().setSellerType(
      addRevisedProductToPDTEvent.isTrustedSeller() ?
        SellerType.TRUSTED_SELLER :
        SellerType.NON_TRUSTED_SELLER);
    productAndReviewerDetails.getProduct().setSellerBadge(
      SellerBadgeConstants.fromSellerBadgeConstants(addRevisedProductToPDTEvent.getSellerBadge()));
    solrVendorCollectionService.publishSolrAddPDTProductBatchEvent(Collections.singletonList(
        new ProductAndReviewerDetailsDTO(productAndReviewerDetails.getProduct(),
            productAndReviewerDetails.getProductReviewer())));
    log.info("product data saved to solr after revision : {}", productAndReviewerDetails.getProduct().getProductCode());
    if (productAndReviewerDetails.getProduct().isPostLive()) {
      this.approvedProductPublisherService.publishRevisedVendorApprovedEvent(productAndReviewerDetails.getProduct(), true);
    }
    if (isEligibleForRevised(productAndReviewerDetails, imageQcProcessedAndBrandResponse)) {
      log.debug("Response for image qc from PBP : {}", imageQcProcessedAndBrandResponse);
      ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
      productImageQcFeedbackRequest.setProductCode(addRevisedProductToPDTEvent.getProductCode());
      productImageQcFeedbackRequest
          .setSystemFeedback(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getImageQcResponse());
      productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
    }
    log.info("com.gdn.pbp.add.revised.product.to.pdt processing done for productCode : {}", productAndReviewerDetails.getProduct().getProductCode());
  }

  public boolean isEligibleForRevised(ProductAndReviewerDetailsDTO productAndReviewerDetails,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) {
    return !ReviewType.CONTENT.equals(productAndReviewerDetails.getProduct().getReviewType()) && Objects.nonNull(
        imageQcProcessedAndBrandResponse) && Objects.nonNull(
        imageQcProcessedAndBrandResponse.getImageQcProcessedResponse());
  }

  @Override
  public void processDimensionsUpdateEvent(PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel)
      throws IOException {
    productService.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel);
  }

  @Override
  public void processAutoApprovalCheckEvent(AutoApprovalTypeRequestModel autoApprovalTypeRequest) {
    try {
      GdnRestSingleResponse<AutoApprovalTypeResponse> autoApprovalTypeResponse = productServiceRepository
          .getAutoApprovalType(autoApprovalTypeRequest.getStoreId(), autoApprovalTypeRequest.getProductCode(), false,
              new AutoApprovalTypeRequest(autoApprovalTypeRequest.getCategoryCode(), autoApprovalTypeRequest.isEdited(),
                  autoApprovalTypeRequest.getReviewType(), autoApprovalTypeRequest.isRevised()));
      if (!autoApprovalTypeResponse.isSuccess() || Objects.isNull(autoApprovalTypeResponse.getValue())) {
        log.error("Failed to get response from PBP while auto approving product : {} ",
            autoApprovalTypeRequest.getProductCode(), autoApprovalTypeResponse.getErrorMessage());
      } else if (Constants.CONTENT_AND_IMAGE.equalsIgnoreCase(
          autoApprovalTypeResponse.getValue().getAutoApprovalType())) {
        PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
            productService.autoApproveProduct(autoApprovalTypeRequest.getProductCode());
        if (Objects.nonNull(publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel())) {
          productService.publishInternalHistoryEventForProduct(
              publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel());
        }
      }
    } catch (Exception e) {
      log.error("Error while processing auto approval criteria check event  : {} ", autoApprovalTypeRequest, e);
    }
  }
}
