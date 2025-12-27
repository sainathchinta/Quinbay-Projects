package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.ProductWfStateResponse;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.entity.ProductWfState;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.repository.SequenceRepository;
import com.gdn.mta.product.service.FileStorageService;
import com.gdn.mta.product.service.ImageProcessorService;
import com.gdn.mta.product.service.EmailNotificationService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductWorkflowService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.publisher.Publisher;
import com.gdn.partners.pbp.publisher.product.ProductPublisherBean;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.newrelic.api.agent.Trace;

@Service(value = "productWfService")
@Transactional(readOnly = true)
public class ProductWorkflowServiceBean implements ProductWfService {

  private static final String APP_NAME = "appName";

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductWorkflowServiceBean.class);

  private static final String BUSINES_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String NOTES = "notes";
  private static final String NEED_REVISION_NOTES = "needRevisionNotes";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_CREATION_TYPE = "productCreationType";
  private static final String REQUEST = "request";
  private static final String NEED_EMAIL_NOTIFICATION = "needEmailNotification";
  private static final List<String> STUCK_PRODUCT_STATUS = Collections
      .unmodifiableList(Arrays.asList(WorkflowStates.PROCESS_IMAGE.getValue(),
          WorkflowStates.CONTENT_APPROVAL.getValue(),
          WorkflowStates.CONTENT_APPROVED.getValue(),
          WorkflowStates.IMAGE_APPROVAL.getValue(),
          WorkflowStates.IMAGE_APPROVED.getValue()));
  private static final String IS_SKIP_NOTIFICATION = "IS_SKIP_NOTIFICATION";
  private static final String MPP_FLOW = "MPPFlow";
  private static final String SLASH = "/";


  @Autowired
  @Qualifier(value = ProductWorkflowProcessorBean.BEAN_NAME + WorkflowProcessor.SUFFIX_BEAN_NAME)
  private WorkflowProcessor workflowProcessor;

  @Autowired
  private SequenceRepository sequenceRepository;

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductWfRepository productWorkflowRepository;

  @Autowired
  private ProductWorkflowService productWorkflowService;

  @Autowired
  private ProductWorkflowRepository oldProductWorkflowRepository;

  @Autowired
  @Qualifier(value = ProductPublisherBean.BEAN_NAME + Publisher.SUFFIX_BEAN_NAME)
  private Publisher publisher;

  @Autowired
  private ProductRepository productRepository;

  @Lazy
  @Autowired
  private ProductService productService;

  @Lazy
  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private EmailNotificationService emailNotificationService;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private ImageProcessorService imageProcessorService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${stuck.product.max.retry.count}")
  private int stuckProductMaxRetryCount;

  @Value("${override.force.review}")
  private boolean overrideForceReview;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  public ProductWorkflowServiceBean(ObjectMapper objectMapper,
      @Value("${workflow.product.flow}") String flow) throws Exception {
    Map<String, List<String>> flows =
        objectMapper.readValue(flow, new TypeReference<Map<String, List<String>>>() {});
    for (Entry<String, List<String>> entry : flows.entrySet()) {
      ProductWorkflowServiceBean.FLOWS.put(entry.getKey(), entry.getValue());
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public String create(String businessPartnerCode, String businessPartnerName, ProductCreationType productCreationType,
      ProductRequest request) throws Exception {
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    String productCode = "MTA-" + org.apache.commons.lang3.StringUtils
        .leftPad(String.valueOf(this.sequenceRepository.findByCode("MTA")), 7, '0');
    request.setCreatedBy(StringUtils.isEmpty(username) ? GdnBaseLookup.DEFAULT_USERNAME : username);
    request.setCreatedDate(Calendar.getInstance().getTime());
    request.setProductCode(productCode);
    request.setCreatedMerchant(businessPartnerCode);
    if (StringUtils.isEmpty(businessPartnerCode)) {
      this.productLevel1WipService.create(businessPartnerCode, businessPartnerName,
          Objects.nonNull(productCreationType) ? productCreationType.getProductCreationType() : StringUtils.EMPTY,
          request);
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(BUSINES_PARTNER_CODE, businessPartnerCode);
      datas.put(BUSINESS_PARTNER_NAME, businessPartnerName);
      datas.put(PRODUCT_CODE, productCode);
      datas.put(PRODUCT_CREATION_TYPE,
          Objects.nonNull(productCreationType) ? productCreationType.getProductCreationType() : StringUtils.EMPTY);
      datas.put(REQUEST, request);
      this.workflowProcessor.process(WorkflowProcessCode.CREATE.getValue(), datas);
    }
    return productCode;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateRejectedProduct(ProductRequest request) throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put(PRODUCT_CODE, request.getProductCode());
    datas.put(REQUEST, request);
    this.workflowProcessor.process(WorkflowProcessCode.UPDATE_REJECTED_PRODUCT.getValue(), datas);
    this.productService.updateRejectedProduct(request);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void createDirect(String productCode) throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put(PRODUCT_CODE, productCode);
    this.workflowProcessor.process(WorkflowProcessCode.CREATE_DIRECT.getValue(), datas);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveDraft(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productWorkflows.isEmpty()) {
      Map<String, Object> publisherDatas = new HashMap<String, Object>();
      publisherDatas.put(APP_NAME, "PDT");
      publisherDatas.put(PRODUCT_CODE, productCode);
      this.productLevel1WipService.approveDraft(productCode, null);
      this.productWorkflowService.submit(storeId, productCode);
      this.publisher.publish(publisherDatas);
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      this.workflowProcessor.process(WorkflowProcessCode.APPROVE_DRAFT.getValue(), datas);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveQC(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productWorkflows.isEmpty()) {
      // do nothing
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      this.workflowProcessor.process(WorkflowProcessCode.APPROVE_QC.getValue(), datas);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveContent(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productWorkflows.isEmpty()) {
      this.productService.approveContent(storeId, productCode, false, false);
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      this.workflowProcessor.process(WorkflowProcessCode.APPROVE_CONTENT.getValue(), datas);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void processImage(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productWorkflows.isEmpty()) {
      this.productWorkflowService.processImage(storeId, productCode, false);
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      this.workflowProcessor.process(WorkflowProcessCode.PROCESS_IMAGE.getValue(), datas);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveImage(String productCode) throws Exception {
      String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
      if (StringUtils.isEmpty(storeId)) {
        LOGGER.warn("StoreId is empty, for approving image of productCode : {}", productCode);
        storeId = Constants.DEFAULT_STORE_ID;
      }
      List<ProductWf> productWorkflows =
          this.productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
      ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
      LOGGER.info("Product workFlows : {} for productCode : {} and skipReview : {}", productWorkflows, productCode,
        productCollection.isSkipReview());
      try {
      if (productWorkflows.isEmpty() || productCollection.isSkipReview()) {
        this.productService.approveImage(storeId, productCode, false);
      } else {
        Map<String, Object> datas = new HashMap<String, Object>();
        datas.put(PRODUCT_CODE, productCode);
        this.workflowProcessor.process(WorkflowProcessCode.APPROVE_IMAGE.getValue(), datas);
      }
    }
    catch (Exception e ){
      LOGGER.error("Exception occured in image approval for product code : {} ", productCode, e);
      productService.setReviewPendingFlagToTrue(storeId, productCode);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void rejectImage(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (productWorkflows.isEmpty()) {
      this.productWorkflowService.rejectProcessImage(storeId, productCode);
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      this.workflowProcessor.process(WorkflowProcessCode.REJECT_IMAGE.getValue(), datas);
    }
  }

  @Trace(dispatcher = true)
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String productCode, String notes, boolean needEmailNotification) throws Exception {
    String storeId = mandatoryParameterHelper.getStoreId();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    ProductCollection response =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (response == null || (!response.isPostLive() && (response.isActivated() && response.isViewable() && !response
        .isEdited()))) {
      throw new ApplicationException(ErrorCategory.INVALID_STATE,
          "at product, but try to delete it with productCode : " + productCode);
    }
    if (productWorkflows.isEmpty()) {
      this.productService.delete(productCode, notes, ProductReviewStatus.SCREENING_REJECTED.name());
    } else {
      Map<String, Object> datas = new HashMap<String, Object>();
      datas.put(PRODUCT_CODE, productCode);
      datas.put(NOTES, notes);
      datas.put(NEED_EMAIL_NOTIFICATION, String.valueOf(needEmailNotification));
      this.workflowProcessor.process(WorkflowProcessCode.DELETE.getValue(), datas);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void resubmit(ProductRequest productRequest, UpdateProductLevel3Wip updateProductLevel3Wip)
      throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put("productRequest", productRequest);
    datas.put("updateProductLevel3Wip", updateProductLevel3Wip);
    datas.put(PRODUCT_CODE, productRequest.getProductCode());
    this.workflowProcessor.process(WorkflowProcessCode.RESUBMIT.getValue(), datas);
  }

  @Override
  public ProductWorkflowStatus status(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = this.productWorkflowRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    ProductWorkflowStatus productWorkflowStatus = new ProductWorkflowStatus();
    ProductCollection productData =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (Objects.nonNull(productData)) {
      productWorkflowStatus.setReviewPending(productData.isReviewPending());
      if (CollectionUtils.isEmpty(productWorkflows)) {
        List<ProductWorkflow> oldProductWorkflows = this.oldProductWorkflowRepository
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productData.getProductId());
        List<String> oldStates = getOldStates(oldProductWorkflows);
        if (oldStates.contains(WorkflowStates.CONTENT_APPROVAL.getValue()) && !oldStates.contains(WorkflowStates.IMAGE_APPROVAL.getValue())
            && !oldStates.contains(WorkflowStates.PROCESS_IMAGE.getValue())) {
          oldStates.add(WorkflowStates.IMAGE_APPROVED.getValue());
        } else if ((oldStates.contains(WorkflowStates.IMAGE_APPROVAL.getValue()) || oldStates.contains(WorkflowStates.PROCESS_IMAGE.getValue()))
            && !oldStates.contains(WorkflowStates.CONTENT_APPROVAL.getValue())) {
          oldStates.add(WorkflowStates.CONTENT_APPROVED.getValue());
        }
        if (!CollectionUtils.isEmpty(oldStates)) {
          for (String oldState : oldStates) {
            List<String> states = ProductWorkflowServiceBean.FLOWS.get(oldState);
            for (String state : states) {
              productWorkflowStatus.getStatus().put(state, true);
            }
          }
        }
        productWorkflowStatus.getStates().addAll(oldStates);
      } else {
        for (ProductWf productWorkflow : productWorkflows) {
          List<String> states = ProductWorkflowServiceBean.FLOWS.get(productWorkflow.getState());
          productWorkflowStatus.getStates().add(productWorkflow.getState());
          if (!CollectionUtils.isEmpty(states)) {
            for (String state : states) {
              productWorkflowStatus.getStatus().put(state, true);
            }
          }
        }
      }
    }
    return productWorkflowStatus;
  }

  @Override
  public void updateAndPublish(Product request, String notes, String brandCode, String brandApprovalStatus)
      throws Exception {
    this.productService.update(request, notes, brandCode, brandApprovalStatus);
    Map<String, Object> datas = new HashMap<>();
    datas.put(APP_NAME, "PDT");
    datas.put(PRODUCT_CODE, request.getProductCode());
    this.publisher.publish(datas);
  }

  @Override
  public void publishDirectProductCreationData(String productCode) throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put(APP_NAME, GdnBaseLookup.APP_NAME_PDT);
    datas.put(PRODUCT_CODE, productCode);
    this.publisher.publish(datas);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void returnForCorrection(List<String> productCodes, String notes, NeedRevisionNotes revisionNotes,
      boolean screeningAction)
      throws Exception {
    for (String productCode : productCodes) {
      returnForCorrection(productCode, notes, revisionNotes, false, screeningAction, true);
    }
  }

  @Override
  public void returnForCorrection(String productCode, String notes, NeedRevisionNotes revisionNotes,
      boolean autoNeedRevision, boolean screeningAction, boolean validateDraftState)
      throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put(PRODUCT_CODE, productCode);
    datas.put(NOTES, notes);
    datas.put(NEED_REVISION_NOTES, revisionNotes);
    datas.put(Constants.AUTO_NEED_REVISION_FLAG, autoNeedRevision);
    datas.put(Constants.SCREENING_ACTION, screeningAction);
    datas.put(Constants.VALIDATE_DRAFT_STATE, validateDraftState);
    this.workflowProcessor.process(WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue(), datas);
    productService.publishProductStatusEventByProductCode(productCode, ProductStatus.NEED_CORRECTION, notes);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(List<String> productCodes, String notes) throws Exception {
    for (String productCode : productCodes) {
      ProductDetailResponse productDetailResponse = productService.findProductDetailByProductCode(productCode, false);
      ProductCollection productCollection = productCollectionRepository
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(mandatoryParameterHelper.getStoreId(), productCode);
      this.delete(productCode, notes, true);
      productService.publishProductStatusEvent(productDetailResponse, productCollection, ProductStatus.REJECTED, notes);
    }
  }

  @Override
  public void create(ProductCreationRequest request, boolean isSkipNotification, boolean MPPFlow) throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put(REQUEST, request);
    datas.put(PRODUCT_CODE, request.getProductCode());
    datas.put(PRODUCT_CREATION_TYPE, Objects.nonNull(request.getProductCreationType()) ?
        request.getProductCreationType().getProductCreationType() :
        StringUtils.EMPTY);
    datas.put(IS_SKIP_NOTIFICATION, isSkipNotification);
    datas.put(MPP_FLOW, MPPFlow);
    if (StringUtils.isEmpty(request.getBusinessPartnerCode())) {
      this.workflowProcessor.process(WorkflowProcessCode.CREATE_PRODUCT_DIRECT.getValue(), datas);
    } else {
      this.workflowProcessor.process(WorkflowProcessCode.CREATE_PRODUCT.getValue(), datas);
    }
    validateProductUpdateRequest(request);
  }

  @Trace(dispatcher=true)
  @Async
  private void validateProductUpdateRequest(ProductCreationRequest request) throws Exception {
    if (StringUtils.isNotEmpty(request.getOldProductCode())) {
      this.delete(request.getOldProductCode(), request.getOldProductRejectionNote(), Boolean.FALSE);

    }
  }

  @Override
  public List<ProductWorkflowStatus> getProductWorkFlowByProductCodes(List<String> productCodes) throws Exception {
    List<ProductWorkflowStatus> productWorkflowStatusList = new ArrayList<ProductWorkflowStatus>();
    Map<String, List<ProductWf>> productCodeAndProductWfList = new HashMap<>();
    GdnPreconditions.checkArgument(!CollectionUtils.isEmpty(productCodes),
        Constants.REQUIRED_PRODUCT_CODES);
    productCodeAndProductWfList = productWorkflowService.getProductWfByProductCodes(productCodes);
    for (Map.Entry<String, List<ProductWf>> productCodeAndProductWf : productCodeAndProductWfList.entrySet()) {
      ProductWorkflowStatus productWorkflowStatus = new ProductWorkflowStatus();
      if (productCodeAndProductWf.getValue().isEmpty())  {
        ProductDetailResponse productData = this.productRepository
            .findProductDetailByProductCode(productCodeAndProductWf.getKey());
        List<ProductWorkflow> oldProductWorkflows = this.oldProductWorkflowRepository
            .findByStoreIdAndProductIdAndMarkForDeleteFalse(GdnMandatoryRequestParameterUtil.getStoreId(), productData.getId());
        List<String> oldStates = getOldStates(oldProductWorkflows);
        if (oldStates.contains(WorkflowStates.CONTENT_APPROVAL.getValue())
            && !oldStates.contains(WorkflowStates.IMAGE_APPROVAL.getValue())
            && !oldStates.contains(WorkflowStates.PROCESS_IMAGE.getValue())) {
          oldStates.add(WorkflowStates.IMAGE_APPROVED.getValue());
        } else if ((oldStates.contains(WorkflowStates.IMAGE_APPROVAL.getValue())
            || oldStates.contains(WorkflowStates.PROCESS_IMAGE.getValue()))
            && !oldStates.contains(WorkflowStates.CONTENT_APPROVAL.getValue())) {
          oldStates.add(WorkflowStates.CONTENT_APPROVED.getValue());
        }
        if (!CollectionUtils.isEmpty(oldStates)) {
          for (String oldState : oldStates) {
            List<String> states = ProductWorkflowServiceBean.FLOWS.get(oldState);
            for (String state : states) {
              productWorkflowStatus.getStatus().put(state, true);
            }
          }
        }
        productWorkflowStatus.getStates().addAll(oldStates);
      } else {
        for (ProductWf productWorkflow : productCodeAndProductWf.getValue()) {
          List<String> states = ProductWorkflowServiceBean.FLOWS.get(productWorkflow.getState());
          productWorkflowStatus.getStates().add(productWorkflow.getState());
          if (CollectionUtils.isNotEmpty(states)) {
            for (String state : states) {
              productWorkflowStatus.getStatus().put(state, true);
            }
          }
        }
      }
      productWorkflowStatus.setProductCode(productCodeAndProductWf.getKey());
      productWorkflowStatusList.add(productWorkflowStatus);
    }
    return productWorkflowStatusList;
  }

  /**
   * get old states of old product-workflows
   *
   * @param oldProductWorkflows must not null
   * @return List<String>
   */
  private List<String> getOldStates(List<ProductWorkflow> oldProductWorkflows) {
    List<String> oldStates = new ArrayList<String>();
    if (!CollectionUtils.isEmpty(oldProductWorkflows)) {
      for (ProductWorkflow oldProductWorkflow : oldProductWorkflows) {
        String oldState;
        switch (oldProductWorkflow.getState()) {
          case ProductWorkflowLookup.STATE_DRAFT:
            oldState = WorkflowStates.DRAFT.getValue();
            break;
          case ProductWorkflowLookup.STATE_REVIEW_CONTENT:
            oldState = WorkflowStates.CONTENT_APPROVAL.getValue();
            break;
          case ProductWorkflowLookup.STATE_REVIEW_IMAGE:
            oldState = WorkflowStates.IMAGE_APPROVAL.getValue();
            break;
          case ProductWorkflowLookup.STATE_PROCESS_IMAGE:
            oldState = WorkflowStates.PROCESS_IMAGE.getValue();
            break;
          case ProductWorkflowLookup.STATE_ACTIVE:
            oldState = WorkflowStates.ACTIVE.getValue();
            break;
          default:
            oldState = null;
            break;
        }
        if (oldState != null) {
          oldStates.add(oldState);
        }
      }
    }
    return oldStates;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public StuckProductResponse getStuckProducts(int retryBatchSizeCount, int retryTimeSpan) {
   Date date = new Date(System.currentTimeMillis() - TimeUnit.MINUTES.toMillis(
        retryTimeSpan));
    Page<ProductWfState> productWfStateList = productWorkflowRepository
        .getProductCodesByState(stuckProductMaxRetryCount, date, PageRequest.of(0, retryBatchSizeCount));
    List<ProductWfState> productWfStateListAboveRetryCount =
        updateRetryCount(productWfStateList);
    List<ProductWfStateResponse> productWfStateResponseListAboveRetryCount =
        ProductWfStateResponseGenerator(productWfStateListAboveRetryCount,
            productWfStateListAboveRetryCount.size());
    emailNotificationService.sendProductStuckAlertMail(productWfStateListAboveRetryCount, retryBatchSizeCount);
    if (Objects.isNull(productWfStateList) || !(productWfStateList.hasContent())) {
      return new StuckProductResponse(null, 0,
          productWfStateResponseListAboveRetryCount);
    }
    List<ProductWfStateResponse> productWfStateResponseList =
        ProductWfStateResponseGenerator(productWfStateList.getContent(), retryBatchSizeCount);
    return new StuckProductResponse(productWfStateResponseList, 0,
        productWfStateResponseListAboveRetryCount);
  }

  /**
   * update the cronJobRetryCount and get ProductWfState of products above retry count
   * @param productWfStateList
   */
  private List<ProductWfState> updateRetryCount(Page<ProductWfState> productWfStateList) {
    String storeId = mandatoryParameterHelper.getStoreId();
    if (Objects.nonNull(productWfStateList) && productWfStateList.hasContent()) {
      List<String> productList =
          productWfStateList.getContent().stream().map(ProductWfState::getProductCode).collect(Collectors.toList());
      productCollectionRepository.updateCronJobRetryCount(storeId, productList);
    }
    return productWorkflowRepository.getProductAboveCronJobRetryCount(storeId, stuckProductMaxRetryCount);
  }

  /**
   * get list of ProductWfStateResponse by sending list of ProductWfState and limit size
   *
   * @param productWfStates
   * @param limitSize
   * @return
   */
  private List<ProductWfStateResponse> ProductWfStateResponseGenerator(List<ProductWfState> productWfStates, Integer limitSize) {
    return productWfStates.stream()
        .limit(limitSize)
        .map(productWfState -> new ProductWfStateResponse(productWfState.getProductCode(),
            productWfState.getState()))
        .collect(Collectors.toList());
  }

  @Override
  @Transactional(readOnly = false)
  public void updateResubmitCountOnProductResubmission(ProductCollection productCollection) {
    this.productCollectionRepository.saveAndFlush(productCollection);
  }

  @Override
  public ProductCollection getProductCollectionByProductCode(String productCode) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode), Constants.REQUIRED_PRODUCT_CODES);
    return productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
  }

  @Override
  public void retryResizeEditedImages(String productCode) throws Exception{
    BulkResizeImageRequest bulkResizeImageRequest = new BulkResizeImageRequest();
    List<ImageRequest> imageRequests = new ArrayList<>();
    ProductDetailResponse productDetailResponse =
        productService.findProductDetailByProductCode(productCode, false);
    bulkResizeImageRequest.setGroupCode(productCode);
    for (Image image : productDetailResponse.getImages()){
      if(image.getOriginalImage() && image.isEdited() && !image.isMarkForDelete()){
        ImageRequest imageRequest = new ImageRequest();
        String[] splitImageFilenameByDash = image.getLocationPath().split(SLASH);
        imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length-1]);
        imageRequest.setHashCode(image.getHashCode());
        imageRequest.setAbsoluteImagePath(fileStorageService.generateFinalImageFullPath(image.getLocationPath()));
        imageRequests.add(imageRequest);
      }
    }
    if(CollectionUtils.isNotEmpty(imageRequests)) {
      bulkResizeImageRequest.setImageRequests(imageRequests);
      imageProcessorService.resizeEditedImageInFileStoreOrGcs(bulkResizeImageRequest);
    }else {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          Constants.NO_EDITED_IMAGES_FOR_RESIZE + " : " + productCode);
    }
  }

  private boolean checkProductQcReviewResponse(String storeId, String productCode) {
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.nonNull(response)) {
      return response.isForceReview();
    }
    return false;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void approveImageForRevisedProduct(String storeId, String productCode, List<ScaleImageResponse> imageResponses, boolean isCategoryChanged,
      List<ProductCategoryResponse> categoryResponseList)
      throws Exception {
    ProductCollection productCollection = getProductCollectionByProductCode(productCode);
    ActivateImageResponse activateImageResponse = null;
    boolean forceReview =
        checkProductQcReviewResponse(productCollection.getStoreId(), productCollection.getProductCode());
    boolean imageScalingSuccessful = true;
    if (CollectionUtils.isNotEmpty(imageResponses)) {
      activateImageResponse = productService.updateActiveImagesAndGetActivateImageResponse(productCode, imageResponses);
      productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(productCode));
      if (!activateImageResponse.isActive()) {
        LOGGER.info("Checking images for scaling for activation of product : {}", productCode);
        ProductDetailResponse productDetailResponse = productOutbound.getImagesForScalingByProductCode(productCode);
        if (CollectionUtils.isNotEmpty(productDetailResponse.getImages())) {
          LOGGER.error("Images found which are yet to be scaled for product code  : {}, images : {}", productCode,
              productDetailResponse.getImages());
          imageScalingSuccessful = false;
        }
      }
    }
    if (CollectionUtils.isEmpty(imageResponses) || imageScalingSuccessful) {
      ProfileResponse profileResponse =
        businessPartnerRepository.filterDetailByBusinessPartnerCode(productCollection.getBusinessPartnerCode());
      forceReview = Optional.of(profileResponse).filter(ProfileResponse::isTrustedSeller)
        .orElse(new ProfileResponse()).isTrustedSeller() ? Boolean.FALSE : forceReview;
      String productSku = productService.isProductActivationNeeded(storeId, productCollection.getProductId());
      if (StringUtils.isNotBlank(productSku)) {
        if(!productCollection.isReviewPending() || !forceReview) {
          productLevel3Service.activateProductOnNeedCorrection(storeId, productSku, profileResponse, categoryResponseList);
        } else if (overrideForceReview) {
          ProductCollection updatedProductCollection =
            productLevel3Service.updateProductCollection(productCollection);
          productLevel3Service.updateSolrProductCollectionDocument(updatedProductCollection);
        }
      }
      productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
      xProductOutbound.generateProductScoreByProductSkuOrProductCode(productSku, productCode, isCategoryChanged);
    } else {
      LOGGER.warn("Images not activated Successfully for product {}", productCode);
      productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.IMAGE_NOT_ACTIVATED_SUCCESSFULLY, null);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "Images not activated Successfully for product" + productCode);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteAllExistingWorkFlowAndCreateNewState(String storeId, String productCode, String state) {
    List<ProductWf> productWfList =
        productWorkflowRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (CollectionUtils.isNotEmpty(productWfList)) {
      productWorkflowRepository.deleteAll(productWfList);
    }
    ProductWf productWf = new ProductWf(productCode, state);
    productWf.setStoreId(storeId);
    productWorkflowRepository.save(productWf);
  }
}
