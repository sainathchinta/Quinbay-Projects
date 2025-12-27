package com.gdn.x.mta.distributiontask.service.impl;

import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import jakarta.annotation.PostConstruct;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;
import com.gdn.partners.pdt.service.distribution.DistributionTaskHistoryService;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.config.BeanConfiguration;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.EditedReviewTypeConstants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.EditedProductService;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.ImageUtils;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EditedProductServiceImpl implements EditedProductService {

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ProductService productService;

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Autowired
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Autowired
  private VendorRepository vendorRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductDistributionTaskRepository distributionTaskRepository;

  @Autowired
  private DistributionTaskHistoryService distributionTaskHistoryService;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private FileStorageService fileStorageService;

  @PostConstruct
  public void init() {
    ImageUtils.setFileStorageService(fileStorageService);
  }

  @Value("${auto.distribute.switch}")
  private Boolean autoDistributeSwitch;

  @Value("${auto.distribute.default.vendorCode}")
  private String autoDistributeDefaultVendorCode;

  @Override
  @Transactional(readOnly = false)
  public Product editProductDetails(String productCode, AddEditedProductToPDTEvent addEditedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) throws Exception {
    ProductDetailResponse productDetailResponse =
        distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME, productCode);
    log.info("The itemCodes fetched from PCB are {}",
        Optional.ofNullable(productDetailResponse.getProductItemResponses())
            .map(items -> items.stream().map(ProductItemResponse::getSkuCode).toList())
            .orElse(Collections.emptyList()));
    Product newProduct = null;
    boolean updateState = false;
    boolean updateTaskCode = false;
    boolean approvedProduct = false;
    Product existingProduct = this.productRepository.findByProductCode(addEditedProductToPDTEvent.getProductCode());
    if (Objects.nonNull(existingProduct)) {
      existingProduct.setSellerType(addEditedProductToPDTEvent.isTrustedSeller() ?
        SellerType.TRUSTED_SELLER :
        SellerType.NON_TRUSTED_SELLER);
      existingProduct.setSellerBadge(
        SellerBadgeConstants.fromSellerBadgeConstants(addEditedProductToPDTEvent.getSellerBadge()));
      existingProduct.setDistributionMappingStatus(
          addEditedProductToPDTEvent.getDistributionMappingStatus());
    }
    if (Objects.isNull(existingProduct)) {
      log.info("Add edited product to vendor : {} ", addEditedProductToPDTEvent.getProductCode());
      newProduct =
          addEditedProductToVendor(addEditedProductToPDTEvent, imageQcProcessedAndBrandResponse, productDetailResponse);
    } else {
      newProduct = ConverterUtil
          .convertProductDetailResponseToProduct(productDetailResponse, addEditedProductToPDTEvent,
              imageQcProcessedAndBrandResponse);
      ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(productCode);
      updateNewProductData(newProduct, existingProduct);

      // By default set edited state as content and image
      newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
      newProduct.setEdited(true);
      newProduct.setRevised(existingProduct.isRevised());

      // Remove MFD = true data
      if (WorkflowState.PASSED.equals(existingProduct.getState()) && existingProduct.isMarkForDelete()) {
        updateTaskCode = true;
        existingProduct.setReviewType(null);
        approvedProduct = true;
      }
      if (Objects.isNull(productReviewer)) {
        productReviewer =
          ProductReviewer.builder().productCode(newProduct.getProductCode()).approvedDate(null)
            .assignedDate(null).approverAssignee(null).build();
        productReviewer.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
      }
      existingProduct = overwriteDeletedData(existingProduct, newProduct, productReviewer);
      if (approvedProduct) {
        productService
            .updateApprovedProductData(existingProduct, newProduct);
      }
      // Determine product state and editedState
      updateState = determineProductState(addEditedProductToPDTEvent, newProduct, existingProduct, approvedProduct,
          productReviewer);

      // Check if product can be moved between prelive - post-live
      checkIfProductConfigIsChanged(existingProduct, newProduct, productReviewer);

      log.info("product edited data, productCode {} , updateState : {}, state : {}, editedState : {}",
          addEditedProductToPDTEvent.getProductCode(), updateState, newProduct.getState(), newProduct.getReviewType());

      if (EditedReviewTypeConstants.CONTENT_EDIT.equals(addEditedProductToPDTEvent.getReviewTypes())
          || EditedReviewTypeConstants.CONTENT_REFRESH.equals(addEditedProductToPDTEvent.getReviewTypes())) {
        this.productWrapperService.updateEditedProductDetails(existingProduct, newProduct, addEditedProductToPDTEvent.getAllModifiedFields());
      } else {
        this.productService.updateEditedProductImageDetails(existingProduct, newProduct);
      }
      newProduct.setCreatedDate(existingProduct.getCreatedDate());
      productReviewerService.save(productReviewer);
    }
    if (updateState && !approvedProduct) {
      ProductDistributionTask productDistributionTask =
          productDistributionTaskService.findByProductId(existingProduct.getId());
      this.productDistributionTaskService.updateState(productDistributionTask, newProduct.getState());
    }
    if (updateTaskCode && Objects.nonNull(newProduct.getCurrentVendor())) {
      generateDistributionTask(existingProduct, newProduct, existingProduct.getCurrentVendor());
    }
    return newProduct;
  }

  private void updateNewProductData(Product newProduct, Product existingProduct) {
    newProduct.setState(existingProduct.getState());
    newProduct.setCurrentVendor(existingProduct.getCurrentVendor());
    existingProduct.setRestrictedKeywordsPresent(newProduct.isRestrictedKeywordsPresent());
    existingProduct.setSellerType(Optional.of(newProduct).map(Product::getSellerType
    ).orElse(null));
    existingProduct.setSellerBadge(Optional.ofNullable(newProduct.getSellerBadge()).orElse(
      SellerBadge.NONE_MERCHANT));
  }

  private boolean determineProductState(AddEditedProductToPDTEvent addEditedProductToPDTEvent, Product newProduct,
      Product existingProduct, boolean approvedProduct, ProductReviewer productReviewer) {
    boolean updateState = false;
    switch (addEditedProductToPDTEvent.getReviewTypes()) {
      case EditedReviewTypeConstants.CONTENT_REFRESH:
        if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.IMAGE
            .equals(existingProduct.getReviewType())) {
          newProduct.setReviewType(ReviewType.IMAGE);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.CONTENT
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && !ReviewType.CONTENT
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
          productReviewer.setAssignedDate(new Date());
        }else if (WorkflowState.PASSED.equals(existingProduct.getState())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          if (ReviewType.CONTENT.equals(existingProduct.getReviewType())) {
            newProduct.setReviewType(ReviewType.CONTENT);
          }
          resetAssignmentData(productReviewer);
          updateState = true;
        }
        break;
      case EditedReviewTypeConstants.IMAGE_REFRESH:
        if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.CONTENT
            .equals(existingProduct.getReviewType())) {
          newProduct.setReviewType(ReviewType.CONTENT);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.IMAGE
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.IMAGE);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && !ReviewType.IMAGE
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
          productReviewer.setAssignedDate(new Date());
        } else if (WorkflowState.PASSED.equals(existingProduct.getState())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          if (ReviewType.IMAGE.equals(existingProduct.getReviewType())) {
            newProduct.setReviewType(ReviewType.IMAGE);
          }
          resetAssignmentData(productReviewer);
          updateState = true;
        }
        break;
      case EditedReviewTypeConstants.CONTENT_EDIT:
         if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.IMAGE
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
          productReviewer.setAssignedDate(new Date());
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.CONTENT
            .equals(existingProduct.getReviewType())) {
            newProduct.setReviewType(ReviewType.CONTENT);
          productReviewer.setApprovedDate(null);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
        } else if (WorkflowState.PASSED.equals(existingProduct.getState())) {
           newProduct.setState(WorkflowState.IN_REVIEW);
           if (ReviewType.CONTENT.equals(existingProduct.getReviewType()) || approvedProduct) {
             newProduct.setReviewType(ReviewType.CONTENT);
           }
           resetAssignmentData(productReviewer);
           updateState = true;
         }
        break;
      case EditedReviewTypeConstants.IMAGE_EDIT:
         if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.CONTENT
            .equals(existingProduct.getReviewType())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState()) && ReviewType.IMAGE
            .equals(existingProduct.getReviewType())) {
            newProduct.setReviewType(ReviewType.IMAGE);
        } else if (WorkflowState.IN_REVIEW.equals(existingProduct.getState())) {
          newProduct.setState(WorkflowState.IN_REVIEW);
          newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
          productReviewer.setApprovedDate(null);
        } else if (WorkflowState.PASSED.equals(existingProduct.getState())) {
           newProduct.setState(WorkflowState.IN_REVIEW);
           if (ReviewType.IMAGE.equals(existingProduct.getReviewType()) || approvedProduct) {
             newProduct.setReviewType(ReviewType.IMAGE);
           }
           resetAssignmentData(productReviewer);
           updateState = true;
         }
        break;
    }
    return updateState;
  }

  private void resetAssignmentData(ProductReviewer productReviewer) {
    productReviewer.setApprovedDate(null);
    productReviewer.setAssignedDate(null);
    productReviewer.setApproverAssignee(null);
  }

  private Product overwriteDeletedData(Product existingProduct, Product newProduct, ProductReviewer productReviewer) {
    if (WorkflowState.PASSED.equals(existingProduct.getState()) && existingProduct.isMarkForDelete()) {
      log.info("overwrite deleted data for edited product: {} ", newProduct.getProductCode());
      existingProduct.setMarkForDelete(false);
      this.updateMFDFlag(existingProduct);
      productReviewer.setMarkForDelete(false);
    }
    return existingProduct;
  }

  private Product addEditedProductToVendor(AddEditedProductToPDTEvent addEditedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse, ProductDetailResponse productDetailResponse)
      throws Exception {
    ScreeningProductApprovalEvent screeningProductApprovalEvent = new ScreeningProductApprovalEvent();
    BeanUtils.copyProperties(addEditedProductToPDTEvent, screeningProductApprovalEvent, "priceInfo");
    if(CollectionUtils.isNotEmpty(addEditedProductToPDTEvent.getPriceInfo())) {
       screeningProductApprovalEvent.setPriceInfo(addEditedProductToPDTEvent.getPriceInfo());
    }
    screeningProductApprovalEvent.setRestrictedKeywordsDetected(addEditedProductToPDTEvent.getRestrictedKeywordsDetected());
    screeningProductApprovalEvent.setTrustedSeller(addEditedProductToPDTEvent.isTrustedSeller());
    Product product = ProductDomainEventModelConverterUtils
        .convertProductDomainEventModelToProduct(productDetailResponse, screeningProductApprovalEvent,
          false, imageQcProcessedAndBrandResponse);
    //set review Type
    product.setReviewType(EditedReviewTypeConstants.CONTENT_EDIT.equals(addEditedProductToPDTEvent.getReviewTypes()) ?
        ReviewType.CONTENT : ReviewType.IMAGE);
    // get vendor data
    Vendor vendor = getVendorData(product);

    if (Objects.nonNull(vendor)) {
      product.setCurrentVendor(vendor);
    }
    ProductReviewer productReviewer =
        ProductReviewer.builder().approverAssignee(null).approvedDate(null).assignedDate(null).build();
    initializeProductData(addEditedProductToPDTEvent, product, productReviewer);
    productService.updateProductNotesForEditedProducts(product, addEditedProductToPDTEvent.getAllModifiedFields());
    ImageUtils.setActiveFlagInProductAndItemImages(product);
    Product newProduct = this.productRepository.save(product);
    this.productReviewerService.save(productReviewer);

    if (Objects.nonNull(vendor)) {
      generateDistributionTask(newProduct, product, vendor);
    }
    return newProduct;
  }

  private void initializeProductData(AddEditedProductToPDTEvent addEditedProductToPDTEvent, Product product,
      ProductReviewer productReviewer) throws IOException {
    product.setState(WorkflowState.IN_REVIEW);
    product.setProductCreatedDate(Calendar.getInstance().getTime());
    productReviewer.setProductCode(product.getProductCode());
    productReviewer.setStoreId(Constants.DEFAULT_STORE_ID);
    BeanUtils.copyProperties(productReviewer, product);
    product.setStoreId(Constants.DEFAULT_STORE_ID);
    product.setEdited(true);

    // set product and edited product state
    getProductState(product, addEditedProductToPDTEvent.getReviewTypes());
  }

  private Vendor getVendorData(Product newProduct) throws Exception {
    String vendorCode;
    if (autoDistributeSwitch) {
      vendorCode = this.autoDistributionConfigurationRepository
          .findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, Arrays
              .asList(newProduct.getCreatedBy(), newProduct.getBusinessPartnerCode(), newProduct.getCategoryCode()));
    } else {
      vendorCode = autoDistributeDefaultVendorCode;
    }
    return this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(vendorCode);
  }

  private void generateDistributionTask(Product product, Product newProduct, Vendor vendor) throws Exception {
    List<ProductDistributionTask> distributionTasks = this.distributionTaskService
        .generateDistributionTaskForProduct(Constants.DEFAULT_STORE_ID, vendor, Collections.singletonList(product),
            newProduct.getState());
    this.distributionTaskRepository.updateProductDistributionTask(Collections.singletonList(product));
    this.distributionTaskRepository.saveAll(distributionTasks);
    this.distributionTaskHistoryService.create(distributionTasks);
  }

  private void getProductState(Product product, String editedState) {
    if (EditedReviewTypeConstants.CONTENT_EDIT.equals(editedState)) {
      product.setState(WorkflowState.IN_REVIEW);
      product.setReviewType(ReviewType.CONTENT);
    } else if (EditedReviewTypeConstants.IMAGE_EDIT.equals(editedState)) {
      product.setState(WorkflowState.IN_REVIEW);
      product.setReviewType(ReviewType.IMAGE);
    }
  }


  private void updateMFDFlag(Product savedProduct) {
    for (ProductItem productItem : savedProduct.getProductItems()) {
      productItem.setMarkForDelete(false);
      productItem.getProductItemImages().forEach(this::setMFDProductItemImage);
      productItem.getProductItemAttributes().forEach(this::setMFDFlagInAttributes);
    }
    savedProduct.getProductAttributes().forEach(productAttribute -> productAttribute.setMarkForDelete(false));
    savedProduct.getProductImages().forEach(productImage -> productImage.setMarkForDelete(false));
  }

  private ProductItemImage setMFDProductItemImage(ProductItemImage productItemImage) {
    productItemImage.setMarkForDelete(false);
    return productItemImage;
  }

  private ProductItemAttribute setMFDFlagInAttributes(ProductItemAttribute productItemAttribute) {
    productItemAttribute.setMarkForDelete(false);
    return productItemAttribute;
  }

  private void checkIfProductConfigIsChanged(Product existingProduct, Product newProduct,
      ProductReviewer productReviewer) {
    if (existingProduct.isPostLive() != newProduct.isPostLive()) {
      existingProduct.setPostLive(newProduct.isPostLive());
      resetAssignmentData(productReviewer);
    }
    if(existingProduct.isForceReview() != newProduct.isForceReview()){
      existingProduct.setForceReview(newProduct.isForceReview());
    }
  }
}
