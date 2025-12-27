package com.gdn.partners.pbp.service.productlevel1;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductLevel3V2Service;
import com.gdn.partners.pbp.commons.constants.Constants;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.dto.productlevel3.NeedRevisionDeletionStatus;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductLevel1CollectionServiceBean implements ProductLevel1CollectionService {

  private static final String DRAFT ="DRAFT";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  public static final String BULK_CLIENT_ID = "x-bulk";
  
  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  @Lazy
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Autowired
  private ProductHistoryRepository productHistoryRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ProductWfRepository productWfRepository;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurityEnabled;

  @Value("${new.nr.deletion.flow.enabled}")
  private boolean newNRFlowEnabled;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(String businessPartnerCode, String businessPartnerName, String productCode, String brandCode,
      String brandApprovalStatus, boolean postLive, String productCreationType) throws Exception {
    ProductDetailResponse productData = this.productRepository.findProductDetailByProductCode(productCode);
    ProductCollection productCollection =
        getProductCollection(businessPartnerCode, businessPartnerName, productData, brandCode, brandApprovalStatus);
    productCollection.setPostLive(postLive);
    productCollection.setReviewPending(true);
    productCollection.setImageResized(false);
    productCollection.setProductCreationType(productCreationType);
    this.productCollectionRepository.save(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductCollection create(String businessPartnerCode, String businessPartnerName,
      ProductDetailResponse productData, String brandCode, String brandApprovalStatus, boolean postLive,
      String productCreationType) throws Exception {
    ProductCollection productCollection =
        getProductCollection(businessPartnerCode, businessPartnerName, productData, brandCode, brandApprovalStatus);
    productCollection.setMarkForDelete(false);
    productCollection.setReviewPending(true);
    productCollection.setPostLive(postLive);
    productCollection.setProductCreationType(productCreationType);
    this.productCollectionRepository.save(productCollection);
    return productCollection;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductCollection create(String businessPartnerCode, String businessPartnerName,
      ProductDetailResponse productData, String brandCode, String brandApprovalStatus, boolean postLive,
      boolean skipReview, String productCreationType, boolean skipScreening, int prioritySeller) throws Exception {
    ProductCollection productCollection =
        getProductCollection(businessPartnerCode, businessPartnerName, productData, brandCode, brandApprovalStatus);
    productCollection.setMarkForDelete(false);
    if (skipScreening) {
      productCollection.setState(IN_PROGRESS);
    }
    productCollection.setReviewPending(true);
    productCollection.setPostLive(postLive);
    productCollection.setSkipReview(skipReview);
    //if the review is skipped, we will not resize image and consider the existing images as resized image/
    productCollection.setImageResized(skipReview);
    productCollection.setProductCreationType(productCreationType);
    productCollection.setPrioritySeller(prioritySeller);
    this.productCollectionRepository.save(productCollection);
    return productCollection;
  }

  private ProductCollection getProductCollection(String businessPartnerCode, String businessPartnerName,
      ProductDetailResponse productData, String brandCode, String brandApprovalStatus) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (ProductCategoryResponse productCategory : productData.getProductCategoryResponses()) {
      if (!productCategory.isMarkForDelete()) {
        categories.add(productCategory.getCategory());
      }
    }
    ProductCollection productCollection = new ProductCollection();
    if (BULK_CLIENT_ID.equals(GdnMandatoryRequestParameterUtil.getClientId())) {
      productCollection.setBulkCreated(true);
    }
    productCollection.setStoreId(storeId);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setProductId(productData.getId());
    productCollection.setProductCode(productData.getProductCode());
    productCollection.setProductName(productData.getName());
    productCollection.setBrand(productData.getBrand());
    productCollection.setBrandCode(brandCode);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.valueOf(brandApprovalStatus));
    productCollection.setCategoryCode(categories.get(0).getCategoryCode());
    productCollection.setCategoryName(categories.get(0).getName());
    productCollection.setBusinessPartnerCode(businessPartnerCode);
    productCollection.setBusinessPartnerName(businessPartnerName);
    productCollection.setActivated(productData.isActivated());
    productCollection.setViewable(productData.isViewable());
    if (GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE.equals(businessPartnerCode)) {
      productCollection.setMarkForDelete(false);
      productCollection.setState("IN_PROGRESS");
    } else {
      productCollection.setMarkForDelete(true);
      productCollection.setState("DRAFT");
    }
    productCollection.setSubmittedDate(productData.getCreatedDate());
    return productCollection;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductDetailResponse productData = this.productRepository.findProductDetailByProductCode(productCode);
    updateProductCollection(productCode, storeId, productData);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateRejectedProduct(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductDetailResponse productData = this.productRepository.findProductDetailByProductCode(productCode, true);
    List<ProductBusinessPartner> productBusinessPartners =
        productBusinessPartnerRepository.findByStoreIdAndProductId(storeId, productData.getId());
    if (!productBusinessPartners.isEmpty()) {
      for (ProductBusinessPartner savedProductBusinessPartner : productBusinessPartners) {
        savedProductBusinessPartner.setMarkForDelete(false);
        for (ProductItemBusinessPartner savedProductItemBusinessPartner : savedProductBusinessPartner
            .getProductItemBusinessPartners()) {
          savedProductItemBusinessPartner.setMarkForDelete(false);
        }
        for (ProductBusinessPartnerAttribute savedProductBusinessPartnerAttribute : savedProductBusinessPartner
            .getProductBusinessPartnerAttributes()) {
          savedProductBusinessPartnerAttribute.setMarkForDelete(false);
        }
      }
      productBusinessPartnerRepository.saveAll(productBusinessPartners);
    }
    ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (productCollection != null) {
      productCollection.setMarkForDelete(false);
      productCollectionRepository.save(productCollection);
    }

    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productData.getId());
    ProductWorkflow productWorkflow =
        new ProductWorkflow(productData.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION, null, productData.getUpdatedBy(),
            productData.getUpdatedDate(), productData.getStoreId());
    productWorkflowRepository.saveAndFlush(productWorkflow);
    ProductHistory addedProductHistory =
        new ProductHistory(productData.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION,
            ProductWorkflowLookup.STATE_CREATE_TO_DRAFT_NOTES.replaceAll("\\{name\\}", productData.getName()), null,
            null, storeId);
    this.productHistoryRepository.saveAndFlush(addedProductHistory);
    if (!CollectionUtils.isEmpty(productWorkflows)) {
      productWorkflowRepository.deleteAll(productWorkflows);
    }
  }

  private void updateProductCollection(String productCode, String storeId, ProductDetailResponse productData)
      throws Exception {
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    for (ProductCategoryResponse productCategory : productData.getProductCategoryResponses()) {
      if (!productCategory.isMarkForDelete()) {
        categories.add(productCategory.getCategory());
      }
    }
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productCollection.setStoreId(storeId);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setProductId(productData.getId());
    productCollection.setProductCode(productData.getProductCode());
    productCollection.setProductName(productData.getName());
    productCollection.setBrand(productData.getBrand());
    productCollection.setCategoryCode(categories.get(0).getCategoryCode());
    productCollection.setCategoryName(categories.get(0).getName());
    productCollection.setActivated(productData.isActivated());
    productCollection.setViewable(productData.isViewable());
    this.productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductCollection approveDraft(String productCode, ProfileResponse profileResponse) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setAssignedTo(GdnMandatoryRequestParameterUtil.getUsername());
    productCollection.setActivated(true);
    productCollection.setViewable(false);
    productCollection.setState("IN_PROGRESS");
    if (productCollection.isRestrictedKeywordsPresent() && !profileResponse.isTrustedSeller()) {
      productCollection.setPostLive(false);
    }
    return this.productCollectionRepository.save(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void activate(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setActivated(true);
    productCollection.setViewable(true);
    productCollection.setState("ACTIVE");
    this.productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    
    if(productCollection == null){
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, 
          "Product collection data is not found for: " + productCode);
    }
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setMarkForDelete(true);
    productCollection.setState("DELETED");
    this.productCollectionRepository.save(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(ProductCollection productCollection) throws Exception {
    productCollection.setUpdatedStepDate(Calendar.getInstance().getTime());
    productCollection.setMarkForDelete(true);
    productCollection.setState("DELETED");
    this.productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void resubmit(ProductRequest product, Date submitDate) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
            product.getProductCode());
    productCollection.setState("DRAFT");
    productCollection.setBrand(product.getBrand());
    productCollection.setBrandCode(product.getBrandCode());
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.valueOf(product.getBrandApprovalStatus()));
    productCollection.setProductName(product.getName());
    productCollection.setSubmittedDate(submitDate);
    for (ProductCategoryRequest productCategory : product.getProductCategories()) {
      if (!productCategory.isMarkForDelete()) {
        productCollection.setCategoryCode(productCategory.getCategory().getCategoryCode());
        productCollection.setCategoryName(productCategory.getCategory().getName());
        break;
      }
    }
    productCollection.setResubmitCount(productCollection.getResubmitCount() + 1);
    productCollection.setNeedRevision(true);
    this.productCollectionRepository.save(productCollection);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void returnDraftForCorrection(String productCode, NeedRevisionNotes needRevisionNotes,
      boolean autoNeedRevision, boolean screeningAction, boolean validateDraftState) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (validateDraftState && screeningAction && !DRAFT.equals(productCollection.getState())) {
      log.error("Screening need revision failed, because product state is invalid, productCode :{} state : {} ",
          productCode, productCollection.getState());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_STATE_INVALID);
    }
    if (DRAFT.equals(productCollection.getState())) {
      productCollection.setAssignedTo(GdnMandatoryRequestParameterUtil.getUsername());
    } else if (IN_PROGRESS.equalsIgnoreCase(productCollection.getState())) {
      productCollection.setActivated(false);
    }
    productCollection.setViewable(false);
    productCollection.setState("NEED_CORRECTION");
    productCollection.setUpdatedStepDate(new Date());
    productCollection.setNeedCorrectionNotes(
        ConverterUtil.toNeedCorrectionNotesForScreening(needRevisionNotes));
    if (autoNeedRevision) {
      productCollection.setAutoNeedRevision(true);
      productCollection.setNeedRevision(true);
      productCollection.setAutoNeedRevisionCount(productCollection.getAutoNeedRevisionCount() + 1);
    }
    this.productCollectionRepository.save(productCollection);
  }

  @Override
  public ProductCollection findByProductId(String productId) {
    return this.productCollectionRepository
        .findByStoreIdAndProductIdAndMarkForDeleteFalse(GdnMandatoryRequestParameterUtil.getStoreId(), productId);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void updateProductStatus(String storeId, ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent) {
    this.productCollectionRepository
        .updateProductStateByProductCode(storeId, productApprovalDetailStatusEvent.getProductCode(),
            productApprovalDetailStatusEvent.getStatus().getEvent(),
            new Date(productApprovalDetailStatusEvent.getStatusTimeStamp()));
  }

  @Override
  public ProductCollection findByProductCode(String storeId, String productCode) {
    return this.productCollectionRepository
        .findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public PostLiveProductCountResponse findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(String storeId,
      Set<String> productIds, String username, String DEFAULT_NOTE, String businessPartnerCode)
      throws ApplicationRuntimeException {
    List<String> productCodes = new ArrayList<>();
    Map<String, NeedRevisionDeletionStatus> failedProductStatusMap = new HashMap<>();
    List<ProductCollection> productCollectionList = new ArrayList<>();
    if (validateBusinessPartnerCodeForSecurityEnabled && newNRFlowEnabled) {
      productCollectionList =
          productCollectionRepository.findByStoreIdAndProductIdInAndMarkForDeleteFalse(storeId,
              productIds);
    } else {
      productCollectionList =
          productCollectionRepository.findPreLiveProductsByStoreIdAndProductIdsAndMarkForDeleteFalse(storeId,
              productIds);
    }
    productCodes = productCollectionList.stream()
        .filter(productCollection -> StringUtils.equals(productCollection.getBusinessPartnerCode(),businessPartnerCode))
        .map(ProductCollection::getProductCode).collect(Collectors.toList());
    if(newNRFlowEnabled) {
      List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequestList =
          productCodes.stream().map(this::formNeedRevisionEligibilityRequest).collect(Collectors.toList());
      List<NeedRevisionEligibilityResponse> needRevisionEligibilityResponses =
          productLevel3V2Service.eligibilityForNeedRevisionDeletion(storeId, needRevisionEligibilityRequestList,false);
      productCodes = needRevisionEligibilityResponses.stream().filter(
              needRevisionEligibilityResponse -> BooleanUtils.isTrue(
                  needRevisionEligibilityResponse.getEligibleForDeletion()))
          .map(NeedRevisionEligibilityResponse::getProductCode).collect(Collectors.toList());
      failedProductStatusMap = getFailedProductStatusMap(needRevisionEligibilityResponses);
    }
    productCodes
        .forEach(productCode -> this.productService.publishProductWipDeleteEvent(productCode, username, DEFAULT_NOTE));
    return new PostLiveProductCountResponse(productIds.size() - productCodes.size(), failedProductStatusMap);
  }

  private Map<String, NeedRevisionDeletionStatus> getFailedProductStatusMap(
      List<NeedRevisionEligibilityResponse> needRevisionEligibilityResponses) {
    Map<String, NeedRevisionDeletionStatus> failedProductStatusMap;
    failedProductStatusMap = needRevisionEligibilityResponses.stream().filter(
        needRevisionEligibilityResponse -> !BooleanUtils.isTrue(
            needRevisionEligibilityResponse.getEligibleForDeletion())).collect(
        Collectors.toMap(NeedRevisionEligibilityResponse::getProductCode,
            needRevisionEligibilityResponse -> new NeedRevisionDeletionStatus(
                needRevisionEligibilityResponse.isPartOfCampaign(),
                needRevisionEligibilityResponse.isActiveOrderHistory())));
    return failedProductStatusMap;
  }

  private NeedRevisionEligibilityRequest formNeedRevisionEligibilityRequest(String productCode) {
    return NeedRevisionEligibilityRequest.builder().productCode(productCode).build();
  }

  @Override
  public String findTop1ProductHistoryNotes(String storeId, String productId) {
    ProductHistory productHistory = productHistoryRepository
        .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, productId);
    return Objects.nonNull(productHistory) ? productHistory.getNotes() : StringUtils.EMPTY;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateAndSendForCorrection(ProductCollection productCollection) {
    ProductCollection updateProductCollection = productCollectionRepository.save(productCollection);
    updateProductWorkFlowState(updateProductCollection.getProductCode());
  }

  private void updateProductWorkFlowState(String productCode) {
    ProductWf productWf = new ProductWf(productCode, WorkflowStates.NEED_CORRECTION.getValue());
    productWf.setStoreId(Constants.DEFAULT_STORE_ID);
    productWfRepository.save(productWf);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductWorkFlowStateForNeedRevision(String productCode) {
    updateProductWorkFlowState(productCode);
  }

  @Override
  public Integer getProductTypeBasedOnProductCodeOrId(String productCode, String productId) {
    return productBusinessPartnerService.getProductTypeBasedOnProductCodeOrId(productCode, productId);
  }
}
