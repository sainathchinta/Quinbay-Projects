package com.gdn.x.mta.distributiontask.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TimeZone;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.dao.api.feign.XBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XInventoryFeign;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.domain.event.model.DeleteOriginalImageEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.dto.L2StockDetailResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.EnumUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.SolrException;
import org.hibernate.Hibernate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.ApproveProductResponseDto;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductWorkflowRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorQuotaCounterRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.GdnBaseEntity;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.NeedRevisionDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.ProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductHistoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductListRequestDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.SimpleStringDTO;
import com.gdn.x.mta.distributiontask.model.dto.StuckProductsDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.model.enums.ApiErrorCode;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.NeedRevisionType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.AppealProductRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.rest.model.response.AppealProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.QuickApprovalResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.AppealProductService;
import com.gdn.x.mta.distributiontask.service.api.ErrorMessages;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import com.gdn.x.mta.distributiontask.service.api.GcsService;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductBusinessPartnerService;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.config.GcsProperties;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.ImageUtils;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
import com.gdn.x.mta.distributiontask.service.impl.util.ValidationUtil;
import com.gdn.x.mta.distributiontask.service.impl.util.VendorUtils;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.OldAndNewPathDomainEventModel;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.google.api.services.youtube.YouTube;
import com.google.common.collect.ImmutableMap;
import jakarta.annotation.PostConstruct;
import javassist.NotFoundException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductServiceImpl implements ProductService {

  private static final String PRODUCT_NOT_FOUND_ERROR = "Product with given productCode not found";
  private static final String STORE_ID_MUST_NOT_BE_NULL = "StoreId cannot be null";
  private static final String STATES_MUST_NOT_BE_EMPTY = "States cannot be empty";
  private static final String PRODUCT_UNASSIGNED_TO_VENDOR_ERROR =
      "Product is not assigned to given Vendor";
  private static final String FLOW3_PRODUCT_ERROR =
      "Product is created from flow 3";
  private static final String CONTENT_AND_IMAGE_APPROVAL = "Content and Image Approved by Vendor ";
  private static final String EDITED_CONTENT_APPROVAL = "Edited Content Approved by Vendor ";
  private static final String EDITED_IMAGE_APPROVAL = "Edited Image Approved by Vendor ";
  private static final String EDITED_CONTENT_AND_IMAGE_APPROVAL = "Edited Content and Image Approved by Vendor ";
  private static final String QUICK_APPROVAL = "Quick Approve ";
  private static final String PRODUCT_REJECT_VENDOR_ERROR =
      "Product can not be rejected as it is not assigned to given Vendor";
  private static final String ASSIGNED_REASON = "Reviewer Assigned to ";
  private static final String ASSIGNED_BY = " Assigned By ";
  private static final String UNASSIGNED_REASON = "Reviewer unassigned by ";
  private static final String DAYS_ADDED = "daysAdded";
  private static final String VENDORS = "vendors";
  private static final String CONTENT = "content";
  private static final String PRODUCT_ALREADY_APPROVED = "Product is already approved";
  private static final String INVALID_PRODUCT_STATE_FOR_NEED_CORRECTION =
      "Invalid product state for need correction";
  private static final int BEFORE_MONTHS = -1;
  private static final String EMPTY_DROPDOWN_LIST_ELEMENT = "NA";
  private static final String UNASSIGN = "unassign";
  private static final String IMAGE = "image";
  private static final String JKT_TIME_ZONE = "Asia/Jakarta";
  private static final String PRODUCT_NOT_FOUND_IN_PBP = "ProductCode not found PBP workflow ";
  private static final String PRODUCT_INVALID_STATE = "Product is in invalid state ";
  private static final String HYPHEN = "-";
  private static final String INVALID_URL = "video url is invalid";
  private static final String PROTECTED_BRAND = "Seller not authorized for this protected brand";
  private static final String BRAND_DOES_NOT_EXIST = "Brand does not exist, provide a valid brand name and try again.";
  private static final String PBP_ERROR_MESSAGE = "Failed to get response from PBP : {}";
  private static final String INVENTORY_ERROR_MESSAGE = "Failed to get response from Inventory : {}";
  public static final String ASSIGNEE = "Assignee";
  private static final String ACTIVE_STATE = "ACTIVE";

  private static final Set<WorkflowState> APPEALED_PRODUCT_STATES =
      Set.of(WorkflowState.IN_REVIEW, WorkflowState.NEED_CORRECTION);

  private static final Map<ReviewType, String> EDITED_REASON_MAP =
      ImmutableMap.<ReviewType, String>builder().put(ReviewType.CONTENT, EDITED_CONTENT_APPROVAL)
          .put(ReviewType.IMAGE, EDITED_IMAGE_APPROVAL)
          .put(ReviewType.CONTENT_AND_IMAGE, EDITED_CONTENT_AND_IMAGE_APPROVAL).build();

  private static final List<WorkflowState> ALLOW_REPLACE_PRODUCT_DATA =
      new ArrayList<WorkflowState>(Arrays.asList(WorkflowState.REJECTED, WorkflowState.UNASSIGNED,
          WorkflowState.IN_REVIEW, WorkflowState.EXCEEDED_SLA, WorkflowState.QC_REJECTED));

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private AppealProductService appealProductService;

  @Autowired
  private VendorUtils vendorUtils;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private TaskHistoryService taskHistoryService;

  @Autowired
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductUtils productUtils;

  @Autowired
  private VendorRepository vendorRepository;

  @Autowired
  private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Autowired
  private TaskHistoryRepository taskHistoryRepository;

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Autowired
  private GcsService gcsService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private XInventoryFeign xInventoryFeign;

  @Autowired
  private XBPFeign xbpFeign;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Value("${stuck.product.summary.email.address}")
  private String sendStuckProductSummaryEmailAddress;

  @Value("${stuck.product.summary.email.address.cc}")
  private String sendStuckProductSummaryEmailAddressCc;

  @Value("${auto.heal.product.data}")
  private boolean autoHealProductData;

  @Value("${auto.heal.product.distribution.task}")
  private boolean autoHealProductDistributionTask;

  @Value("${validate.product.reject}")
  private boolean validateProductReject;

  @Value("${decrement.vendor.quota.counter}")
  private boolean decrementVendorQuotaCounter;

  @Value("${validate.approved.product.rejection}")
  private boolean validateApprovedProductRejection;

  @Value("${validate.heal.quick.approve.enabled}")
  private boolean validateAndHealQuickApprovalEnabled;

  @Value("${auto.heal.auto.approval.product.data}")
  private boolean autoHealAutoApprovalProductData;

  @Value("${max.image.count.gcs.file.exists}")
  private int maxImageCountForGcsFileCheck;

  @Value("${brand.Attribute.Code}")
  private String brandAttributeCode;
  @Value("${refresh.product.image.details}")
  private boolean refreshProductImageDetails;

  @Value("${delete.original.images}")
  private boolean deleteOriginalImages;

  @Value("${auto.solr.reindexing}")
  private boolean autoSolrReindexingEnabled;

  @Value("${consider.activated.before.autoheal}")
  private boolean considerActivatedBeforeForAutoHeal;

  @Value("${relax.equality.check.mfd.true.item.autoheal}")
  private boolean relaxEqualityCheckForMFDTrueItemAutoHeal;

  @Value("${qc.products.retry.count}")
  private int qcProductsRetryCount;

  @Value("${delete.images.event.based.enabled}")
  private boolean deleteImagesEventBasedEnabled;

  @Value("${warehouse.merchant.commission.type.list}")
  private String warehouseMerchantCommissionTypeList;

  @Value("${warehouse.stock.check.switch}")
  private boolean warehouseStockCheckSwitch;

  @Value("${brand.exist.check.enabled}")
  private boolean brandExistCheckEnabled;

  @Autowired
  private SolrReindexPublisherService solrReindexPublisherService;

  @Autowired
  private YouTube youTube;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Autowired
  private ObjectMapper mapper;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @Autowired
  private ProductActionRetryService productActionRetryService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Value("${pdt.history.update.event}")
  private boolean pdtHistoryUpdateThroughEvent;

  @Value("${resize.image.prefix}")
  private String resizeImagePrefix;

  @Value("${orphaned.image.autoheal.flag}")
  private boolean orphanedImageAutoHealFlag;

  @Value("${orphaned.image.quick.approval.autoheal.flag}")
  private boolean orphanedImageQuickApprovalAutoHealFlag;

  @PostConstruct
  public void init() {
    ImageUtils.setFileStorageService(fileStorageService);
  }

  @Transactional
  public ApproveProductResponseDto approveProductByVendor(Product product, String vendorCode, String notes, boolean isQuickApproval,
      ProductReviewer productReviewer, boolean isBulkAction) throws Exception {
    ApproveProductResponseDto approveProductResponseDto = new ApproveProductResponseDto();
    GdnPreconditions.checkArgument(product != null, PRODUCT_NOT_FOUND_ERROR);
    GdnPreconditions.checkArgument(vendorCode.equals(product.getCurrentVendor().getVendorCode()),
            PRODUCT_UNASSIGNED_TO_VENDOR_ERROR);
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    ProductDistributionTask productDistributionTask =
        this.productDistributionTaskService.findByProductId(product.getId());
    if (productDistributionTask != null) {
      WorkflowState workflowState = getWorkflowStatePostApproval(product);
      product.setState(workflowState);
      // Create separate entry for current state task for auditing
      if (WorkflowState.PASSED.equals(workflowState)) {
        StringBuilder reason;
        if (product.isEdited()) {
          reason = new StringBuilder().append(EDITED_REASON_MAP.get(product.getReviewType()))
              .append(product.getCurrentVendor().getName());
        } else if (isQuickApproval && !isBulkAction) {
          reason = new StringBuilder().append(QUICK_APPROVAL);
        } else if (isQuickApproval) {
          reason = new StringBuilder().append(Constants.BULK_APPROVAL);
        } else {
          reason = new StringBuilder().append(CONTENT_AND_IMAGE_APPROVAL).append(product.getCurrentVendor().getName());
        }
        if (StringUtils.isNotBlank(notes)) {
          reason.append(HYPHEN).append(notes);
        }
        approveProductResponseDto.setReason(reason.toString());
        approveProductResponseDto.setWorkFlowState(workflowState);
        approveProductResponseDto.setPublishHistoryEvent(true);
        approveProductResponseDto.setTaskCode(productDistributionTask.getTaskCode());
      }
      this.productDistributionTaskService.updateState(productDistributionTask, workflowState);
      if (product.isEdited()) {
        GdnBaseRestResponse response =
            pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(),
                product.getReviewType().name());
        if (!response.isSuccess()) {
          log.error(PBP_ERROR_MESSAGE, response.getErrorMessage());
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
        }
      }
      this.productUtils.regenerateProductImageDetails(product);
      productReviewer.setApprovedDate(new Date());
      if (isBulkAction) {
        productReviewer.setApproverAssignee(username);
        productReviewer.setAssignedDate(new Date());
      }
      productReviewerService.save(productReviewer);
      product = this.productRepository.save(product);
      if (decrementVendorQuotaCounter) {
        productFinalApprovalByVendor(product);
      }
      log.info("product details of product code : {} , {} ", product.getProductCode(), product.getState());
      approveProductResponseDto.setProduct(product);
      return approveProductResponseDto;
    } else {
      throw new IllegalStateException(
          "Task not found for this product code " + product.getProductCode());
    }
  }

  @Transactional
  public Product clearProductDetails(Product oldProduct) {
    log.info("Clearing Product Details Product Code {}", oldProduct.getProductCode());
    if (!CollectionUtils.isEmpty(oldProduct.getProductImages())) {
      this.productImageRepository.deleteAll(oldProduct.getProductImages());
    }

    if (!CollectionUtils.isEmpty(oldProduct.getProductItems())) {
      this.productItemRepository.deleteAll(oldProduct.getProductItems());
    }

    if (!CollectionUtils.isEmpty(oldProduct.getProductAttributes())) {
      this.productAttributeRepository.deleteAll(oldProduct.getProductAttributes());
    }

    oldProduct.getProductImages().clear();
    oldProduct.getProductItems().clear();
    oldProduct.getProductAttributes().clear();

    return oldProduct;
  }

  @Override
  public Map<String, Object> countAllProductDetailsWithMultipleFilter(Boolean includeStatus, Boolean includeVendors,
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO) throws IOException, SolrServerException {
    Map<String, Object> countMap = new HashMap<>();
    if (!includeStatus && !includeVendors) {
      countMap.put(DAYS_ADDED,
          solrVendorCollectionService.getFinalQcCounts(distributionTaskMultipleFilterDTO.getStoreId()));
    }
    if (includeStatus) {
      solrVendorCollectionService.getDistributionListCounts(distributionTaskMultipleFilterDTO.getStoreId(), countMap);
    }
    if (includeVendors) {
      countMap.put(VENDORS, vendors(distributionTaskMultipleFilterDTO));
    }
    return countMap;
  }

  @Override
  @Transactional
  public Product createProduct(Product product) {
    log.info("Create new Product Code {}", product.getProductCode());
    if (this.productRepository
        .findByProductCodeAndMarkForDeleteFalse(product.getProductCode()) == null) {
      ImageUtils.setActiveFlagInProductAndItemImages(product);
      return this.productRepository.saveAndFlush(product);
    } else {
      log.error("Product Code {} is already exist !!", product.getProductCode());
      throw new IllegalArgumentException(
          "Product Code " + product.getProductCode() + " is already exist !!");
    }
  }

  @Override
  public boolean getEditedByMerchant(String productCode, long version) {
    Product product = this.productRepository.findByProductCodeAndVersionAndMarkForDeleteFalse(
        productCode, version);
    return Objects.isNull(product);
  }

  @Override
  @Transactional
  public Product findByProductId(String productId) throws Exception {
    Product product = this.productRepository.findByIdAndMarkForDeleteFalse(productId);
    validateProductInfo(product, null);
    this.productUtils.initializeAllProductDetails(product);
    return product;
  }

  @Override
  public Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(WorkflowState workflowState,
      String searchCriteria, Pageable pageable, boolean isSearch, String storeId) throws Exception {
    Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers;
    List<WorkflowState> workflowStateList = new ArrayList<WorkflowState>();
    workflowStateList.add(WorkflowState.PASSED);
    if (workflowState != null) {
      workflowStateList.add(workflowState);
    }
    if (isSearch) {
      String searchKeyWord = searchCriteria != null ? searchCriteria.toLowerCase() : searchCriteria;
      productBusinessPartnerMappers = this.solrVendorCollectionService
          .findProductBusinessPartnerMapper(workflowStateList, searchKeyWord, pageable, storeId);
    } else {
      productBusinessPartnerMappers =
          this.solrVendorCollectionService.findProductBusinessPartnerMapper(workflowStateList, null, pageable, storeId);
    }
    return productBusinessPartnerMappers;
  }

  @Override
  public List<VendorProductStatusDTO> findProductStatusForVendor(Vendor vendor, String storeId) {
    Date todayStart = DateUtils.addMilliseconds(DateUtils.ceiling(new Date(), Calendar.DATE), 0);
    List<VendorProductStatusDTO> vendorProductStatusDTOs =
        this.productRepository.findAllProductStatusForVendor(vendor, storeId,
            DateUtils.addMonths(DateUtils.addDays(todayStart, -1), BEFORE_MONTHS));
    return vendorProductStatusDTOs;
  }

  @Override
  @Transactional(readOnly = true)
  public Product getAllProductDetailsByCode(String code) throws Exception {
    try {
      Product product = this.productRepository.findByProductCodeAndMarkForDeleteFalse(code);
      if (autoSolrReindexingEnabled && Objects.isNull(product)) {
        performSolrAutoReindexing(code);
      }
      sortProductImagesBySequenceId(product);
      sortProductItemImages(product);
      validateProductInfo(product, code);
      this.productUtils.initializeAllProductDetails(product);
      return product;
    } catch (Exception e) {
      log.error("error while Product retrieval. productCode: {} ", code, e);
      throw e;
    }
  }

  private void performSolrAutoReindexing(String productCode) {
    PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel =
        new PDTProductSolrDeleteDomainEventModel();
    pdtProductSolrDeleteDomainEventModel.setProductCodes(Collections.singletonList(productCode));
    log.info("Publishing combined update to solr event for productCode {} , pdtProductSolrDeleteDomainEventModel {} ",
        productCode, pdtProductSolrDeleteDomainEventModel);
    solrReindexPublisherService
        .publishPDTProductSolrBatchDeleteDomainEventModelForReindex(pdtProductSolrDeleteDomainEventModel);
  }

  @Override
  @Transactional(readOnly = false)
  public Product autoHealProductData(Product product, String type) throws Exception {
    if (autoHealProductData) {
      boolean activatedBefore = false;
      boolean isProductDataUpdated = false;
      if (CollectionUtils.isEmpty(product.getProductImages()) || CollectionUtils.isEmpty(product.getProductAttributes())
          || CollectionUtils.isEmpty(product.getProductItems()) || product.getProductItems().stream()
          .anyMatch(productItem -> CollectionUtils.isEmpty(productItem.getProductItemImages()))
          || product.getProductItems().stream().anyMatch(productItem -> CollectionUtils.isEmpty(productItem.getProductItemAttributes()))
          || hasNoMFDFalseImageOrVariantsOrAttributes(product)) {
        ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto =
          ConverterUtil.setProductDataAutoFixHistoryDto(product.getProductCode(), type, StringUtils.EMPTY);
        log.info("Product data is incomplete, triggering autoHeal, productCode : {} ",
          product.getProductCode());
        ProductDetailResponse productDetailResponse =
          productServiceRepository.getProductDetailByProductCode(product.getProductCode(), true,
            true);
        // when auto heal product is true , fetch all the images ignoring MFD check
        Product productFromPcb = ProductDomainEventModelConverterUtils.convertProductDomainEventModelToProduct(
          productDetailResponse, new ScreeningProductApprovalEvent(), true, new ImageQcProcessedAndBrandResponse());
        ProductWorkflowStatusResponse productWorkflowStatusResponse =
          this.productBusinessPartnerService.getWorkflowStatus(product.getProductCode());
        if (considerActivatedBeforeForAutoHeal && Optional.ofNullable(productWorkflowStatusResponse)
          .map(ProductWorkflowStatusResponse::getStates).orElse(Collections.emptyList()).stream()
          .anyMatch(ACTIVE_STATE::equals) && (product.isEdited() || product.isRevised())) {
          activatedBefore = true;
        }
//        setProductAndItemImageResponse(productFromPcb, productDetailResponse);
        if (isEligibleForHealingProductImages(product)) {
          autoHealProductImages(product, productFromPcb, productDataAutoFixHistoryDto, activatedBefore);
        }
        if (isEligibleForHealingAttributes(product)) {
          autoHealAttributes(product, productFromPcb);
          productDataAutoFixHistoryDto.setAdditionalInfo(
              productDataAutoFixHistoryDto.getAdditionalInfo().concat(Constants.PRODUCT_ATTRIBUTE_MISSING));
        }
        if (CollectionUtils.isEmpty(product.getProductItems()) || Optional.of(
            product.getProductItems()).orElse(Collections.emptyList()).stream()
          .noneMatch(Predicate.not(ProductItem::isMarkForDelete))) {
          log.info("Auto Healing product for Empty Item or All MFD true L4s for product : {} ",
            product.getProductCode());
          for (ProductItem productItem : productFromPcb.getProductItems()) {
            productItem.setProduct(product);
            productItem.setId(null);
          }
          autoHealAttributes(product, productFromPcb);
          autoHealProductImages(product, productFromPcb, productDataAutoFixHistoryDto,
            activatedBefore);
          healProductItemImages(product, productFromPcb, true, activatedBefore);
          healProductItemAttributes(product, productFromPcb, true);
          product.setProductItems(productFromPcb.getProductItems());
          productDataAutoFixHistoryDto.setAdditionalInfo(
              productDataAutoFixHistoryDto.getAdditionalInfo().concat(Constants.PRODUCT_ITEM_MISSING));
        }
        if (productContainsEmptyItemImages(product)) {
          healProductItemImages(product, productFromPcb, false, activatedBefore);
          productDataAutoFixHistoryDto.setAdditionalInfo(productDataAutoFixHistoryDto.getAdditionalInfo()
            .concat(Constants.PRODUCT_IMAGES_FROM_PRODUCT_ITEM_MISSING));
        }
        if (productUtils.productContainsEmptyItemAttributes(product)) {
          healProductItemAttributes(product, productFromPcb, false);
          productDataAutoFixHistoryDto.setAdditionalInfo(productDataAutoFixHistoryDto.getAdditionalInfo()
              .concat(Constants.PRODUCT_ITEM_ATTRIBUTES_FROM_PRODUCT_ITEM_MISSING));
        }
        kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, product.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDto));
        log.info("Publishing Kafka event for reconciled revised product of product : {} ",
            product.getProductCode());
        isProductDataUpdated = true;
      }
      if (orphanedImageAutoHealFlag) {
        try {
          isProductDataUpdated = validateAndProcessOrphanedImages(product) || isProductDataUpdated;
        }
        catch (Exception e){
          log.error("Error in the processing of orphaned images for the product:{} with error - ",
              product.getProductCode(), e);
        }
      }
      if(isProductDataUpdated){
        return this.productRepository.saveAndFlush(product);
      }
    }
    return product;
  }



  private static void autoHealAttributes(Product product, Product productFromPcb) {
    for (ProductAttribute productAttribute : productFromPcb.getProductAttributes()) {
      productAttribute.setProduct(product);
      productAttribute.setId(null);
    }
    product.setProductAttributes(productFromPcb.getProductAttributes());
  }

  public void healProductItemImages(Product product, Product productFromPcb, boolean isItemDeleted,
    boolean activatedBefore) {
    // Check if relaxation condition is true (sets have different sizes)
    boolean relaxEqualityCheck =
      relaxEqualityCheckForMFDTrueItemAutoHeal && product.getProductItems().stream().filter(Predicate.not(ProductItem::isMarkForDelete))
        .collect(Collectors.toSet()).size() != productFromPcb.getProductItems().stream()
        .filter(Predicate.not(ProductItem::isMarkForDelete)).collect(Collectors.toSet()).size();
    for (ProductItem productItemPcb : productFromPcb.getProductItems()) {
      ProductItem matchingProductItem = product.getProductItems().stream()
        .filter(item -> productItemPcb.getSkuCode().equals(item.getSkuCode()))
        .findFirst().orElse(null);


      if (Objects.nonNull(matchingProductItem) || relaxEqualityCheck) {
        copyProductItemImagesFromPCB(productItemPcb, matchingProductItem, product,
          isItemDeleted, activatedBefore, relaxEqualityCheck);
      }
    }
  }

  private void healProductItemAttributes(Product product, Product productFromPcb, boolean isItemDelete) {
    for (ProductItem productItemPcb : productFromPcb.getProductItems()) {
      product.getProductItems().stream()
          .filter(productItem -> productItemPcb.getSkuCode().equals(productItem.getSkuCode()))
          .forEach(productItem -> productUtils.copyProductItemAttributesFromPCB(productItemPcb, productItem, isItemDelete));
    }
  }


  private static boolean isEligibleForHealingAttributes(Product product) {
    return CollectionUtils.isEmpty(product.getProductAttributes()) || Optional.ofNullable(
        product.getProductAttributes()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductAttribute::isMarkForDelete));
  }

  private static boolean isEligibleForHealingProductImages(Product product) {
    return CollectionUtils.isEmpty(product.getProductImages()) || Optional.ofNullable(
        product.getProductImages()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductImage::isMarkForDelete));
  }

  private void autoHealProductImages(Product product, Product productFromPcb,
    ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto, boolean activatedBefore) {
      // If none of the Images are MFD false in PDT , proceed with auto heal
      for (ProductImage productImage : productFromPcb.getProductImages()) {
        productImage.setProduct(product);
        productImage.setId(null);
      }
      if(activatedBefore){
        List<ProductImage> nonDeletedProductImages = productFromPcb.getProductImages().stream()
          .filter(Predicate.not(ProductImage::isMarkForDelete))
          .collect(Collectors.toList());
        product.setProductImages(nonDeletedProductImages);
        log.info("Healed product Image for once activated product : {} ", product.getProductCode());
      }
      else {
      // Resized Images will be MFD true in PCB but they be made MFD false before adding it to PDT
      List<ProductImage> productResizedImages = productFromPcb.getProductImages().stream()
        .filter(productImage -> StringUtils.isNotEmpty(productImage.getLocationPath()))
        .filter(productImage -> isResized(productImage.getLocationPath(), resizeImagePrefix))
        .peek(productImage -> productImage.setMarkForDelete(false)).collect(Collectors.toList());

      productFromPcb.getProductImages().removeIf(productImage -> isResized(
        Optional.ofNullable(productImage.getLocationPath()).orElse(StringUtils.EMPTY),
        resizeImagePrefix));

      productFromPcb.getProductImages().removeIf(ProductImage::isActive);
      productFromPcb.getProductImages().addAll(productResizedImages);
      productFromPcb.getProductImages().removeIf(ProductImage::isMarkForDelete);
      product.setProductImages(productFromPcb.getProductImages());
      log.info("Healed product images for never activated product : {} ", product.getProductCode());
    }
    productDataAutoFixHistoryDto.setAdditionalInfo(Constants.PRODUCT_IMAGE_MISSING);
  }

  private boolean validateAndProcessOrphanedImages(Product product) {
    Map<String, ProductImage> originalImageNamesToImageMap = product.getProductImages().stream()
        .filter(productImage -> BooleanUtils.isTrue(productImage.getOriginalImage()))
        .filter(ProductImage::isEdited).filter(ProductImage::isActive).collect(
            Collectors.toMap(productImage -> getImageNames(productImage.getLocationPath()),
                Function.identity(), (v1, v2) -> v2));

    Set<String> resizedEditedImageNames = product.getProductImages().stream()
        .filter(productImage -> BooleanUtils.isFalse(productImage.getOriginalImage()))
        .map(ProductImage::getLocationPath)
        .filter(locationPath -> isResized(locationPath, resizeImagePrefix)).map(this::getImageNames)
        .collect(Collectors.toSet());


    boolean isProductDataUpdated =
        processOrphanedImages(originalImageNamesToImageMap, resizedEditedImageNames);

    Map<String, ProductItemImage> originalItemImageNamesToImageMap = new HashMap<>();
    Set<String> resizedEditedItemImageNames = new HashSet<>();

    product.getProductItems()
        .forEach(productItem -> productItem.getProductItemImages().forEach(productItemImage -> {
          if (BooleanUtils.isTrue(productItemImage.getOriginalImage())
              && productItemImage.isEdited() && productItemImage.isActive()) {
            originalItemImageNamesToImageMap.put(getImageNames(productItemImage.getLocationPath()),
                productItemImage);
          }
          if (BooleanUtils.isFalse(productItemImage.getOriginalImage()) && isResized(
              productItemImage.getLocationPath(), resizeImagePrefix)) {
            resizedEditedItemImageNames.add(getImageNames(productItemImage.getLocationPath()));
          }
        }));
    return processOrphanedItemImages(originalItemImageNamesToImageMap, resizedEditedItemImageNames)
        || isProductDataUpdated;
  }

  private boolean processOrphanedImages(Map<String, ProductImage> originalEditedImages,
      Set<String> resizedEditedImageNames) {
    boolean isProductDataUpdated = false;
    for (Map.Entry<String, ProductImage> productImageEntry : originalEditedImages.entrySet()) {
      if (!resizedEditedImageNames.contains(productImageEntry.getKey())) {
        log.info("Copying final productImage with path:{} to source image bucket",
            productImageEntry.getValue().getLocationPath());
        gcsService.copyImage(gcsProperties.getFinalImageBucketName(),
            (gcsProperties.getFinalImageDirectory() + File.separator
                + gcsProperties.getFinalFullImageDirectory() + File.separator
                + productImageEntry.getValue().getLocationPath()).replaceAll("//", "/"),
            gcsProperties.getSourceImageBucketName(),
            (gcsProperties.getSourceImageDirectory() + File.separator + productImageEntry.getValue()
                .getLocationPath()).replaceAll("//", "/"));
        productImageEntry.getValue().setOriginalImage(Boolean.FALSE);
        productImageEntry.getValue().setActive(Boolean.FALSE);
        productImageEntry.getValue().setEdited(Boolean.FALSE);
        isProductDataUpdated = true;
      }
    }
    return isProductDataUpdated;
  }

  private boolean processOrphanedItemImages(
      Map<String, ProductItemImage> originalEditedItemImageNamesToImageMap,
      Set<String> resizedEditedItemImageNames) {
    boolean isProductDataUpdated = false;
    for (Map.Entry<String, ProductItemImage> productItemImageEntry :
        originalEditedItemImageNamesToImageMap.entrySet()) {
      if (!resizedEditedItemImageNames.contains(productItemImageEntry.getKey())) {
        log.info("Copying final productItemImage with path:{} to source image bucket",
            productItemImageEntry.getValue().getLocationPath());
        gcsService.copyImage(gcsProperties.getFinalImageDirectory(),
            productItemImageEntry.getValue().getLocationPath(),
            gcsProperties.getSourceImageBucketName(),
            productItemImageEntry.getValue().getLocationPath());
        isProductDataUpdated=true;
        productItemImageEntry.getValue().setOriginalImage(Boolean.FALSE);
        productItemImageEntry.getValue().setActive(Boolean.FALSE);
        productItemImageEntry.getValue().setEdited(Boolean.FALSE);
      }
    }
    return isProductDataUpdated;
  }

  private String getImageNames(String locationPath) {
    return locationPath.substring(locationPath.lastIndexOf(Constants.DELIMITER_SLASH) + 1,
        locationPath.indexOf(Constants.CONSTANT_DOT));
  }

  private static boolean isResized(String path, String resizeImagePrefix) {
    List<String> imageResizePrefix = Arrays.asList(resizeImagePrefix.split(Constants.COMMA));
    return StringUtils.isNotEmpty(path) && imageResizePrefix.stream().anyMatch(path::startsWith);
  }


  static boolean hasNoMFDFalseImageOrVariantsOrAttributes(Product product) {
    return Optional.ofNullable(product.getProductImages()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductImage::isMarkForDelete)) || Optional.ofNullable(
        product.getProductItems()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductItem::isMarkForDelete)) || Optional.of(
        product.getProductItems()).orElse(new ArrayList<>()).stream().map(
        productItem -> Optional.ofNullable(productItem.getProductItemImages())
          .orElse(new ArrayList<>())).flatMap(List::stream)
      .noneMatch(Predicate.not(GdnBaseEntity::isMarkForDelete)) || Optional.ofNullable(
        product.getProductAttributes()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductAttribute::isMarkForDelete)) || Optional.of(
            product.getProductItems()).orElse(new ArrayList<>()).stream().map(
            productItem -> Optional.ofNullable(productItem.getProductItemAttributes())
                .orElse(new ArrayList<>())).flatMap(List::stream)
        .noneMatch(Predicate.not(GdnBaseEntity::isMarkForDelete));
  }

  private boolean productContainsEmptyItemImages(Product product) {
    // If none of the Item Images are MFD false in PDT , proceed with auto heal
    return product.getProductItems().stream().anyMatch(
      productItem -> CollectionUtils.isEmpty(productItem.getProductItemImages())
        || product.getProductItems().stream()
        .filter(productItems -> CollectionUtils.isNotEmpty(productItems.getProductItemImages()))
        .flatMap(productItems -> productItem.getProductItemImages().stream())
        .noneMatch(Predicate.not(ProductItemImage::isMarkForDelete)));
  }

  public static void setProductAndItemImageResponse(Product productFromPcb,
    ProductDetailResponse productDetailResponse) {
    productFromPcb.setProductImages(new ArrayList<>());
    for (Image image : productDetailResponse.getImages()) {
      ProductImage productImage = new ProductImage();
      BeanUtils.copyProperties(image, productImage, "product");
      productImage.setProduct(productFromPcb);
      productFromPcb.getProductImages().add(productImage);
    }
    Map<String, ProductItem> skuCodeToProductItemMap = new HashMap<>();
    for (ProductItem productItem : productFromPcb.getProductItems()) {
      skuCodeToProductItemMap.put(productItem.getSkuCode(), productItem);
    }
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      for (Image image : productItemResponse.getImages()) {
        ProductItemImage productItemImage = new ProductItemImage();
        String skuCodeToMatch = productItemResponse.getSkuCode();
        ProductItem matchingProductItem =
          skuCodeToProductItemMap.getOrDefault(skuCodeToMatch, null);
        BeanUtils.copyProperties(image, productItemImage);
        productItemImage.setProductItem(matchingProductItem);
      }
    }
  }

  private void copyProductItemImagesFromPCB(ProductItem productItemPcb, ProductItem productItem,
    Product product, boolean isItemDeleted, boolean activatedBefore, boolean relaxEqualityCheck) {
    if(Objects.isNull(productItem)){
      ProductItem newProductItem = new ProductItem();
      BeanUtils.copyProperties(productItemPcb, newProductItem);
      newProductItem.setId(null);
      newProductItem.setProduct(product);
      productItem = newProductItem;
    }
    List<ProductItemImage> productItemNonDeletedImages =
      productItemPcb.getProductItemImages().stream().filter(Predicate.not(ProductItemImage::isMarkForDelete)).collect(Collectors.toList());
    List<ProductItemImage> productItemImagesFromPCB = new ArrayList<>(productItemPcb.getProductItemImages());
    if(activatedBefore){
      productItemImagesFromPCB = new ArrayList<>(productItemNonDeletedImages);
      log.info("Healed variant Image for once Activated product : {} ", product.getProductCode());
    }
    else {
      // Resized Images will be MFD true in PCB , they be made MFD false before adding it to PDT
      List<ProductItemImage> productItemResizedImages =
        productItemImagesFromPCB.stream().filter(itemImage -> StringUtils.isNotEmpty(itemImage.getLocationPath()))
        .filter(productItemImage -> isResized(productItemImage.getLocationPath(), resizeImagePrefix))
        .peek(productImage -> productImage.setMarkForDelete(false)).collect(Collectors.toList());
      productItemImagesFromPCB.removeIf(productImage -> isResized(StringUtils.defaultIfEmpty(productImage.getLocationPath(), StringUtils.EMPTY),
        resizeImagePrefix));
      productItemImagesFromPCB.removeIf(ProductItemImage::isActive);
      productItemImagesFromPCB.addAll(productItemResizedImages);
      productItemImagesFromPCB.removeIf(ProductItemImage::isMarkForDelete);
      if(relaxEqualityCheck){
        productItemPcb.setProductItemImages(productItemImagesFromPCB);
      }
    }
    productItem.setProductItemImages(productItemImagesFromPCB);
    for (ProductItemImage productItemImage : productItemImagesFromPCB) {
      productItemImage.setId(null);
      productItemImage.setProductItem(isItemDeleted ? productItemPcb : productItem);
      log.info("Healed variant Image for never activated product : {} ", product.getProductCode());
    }
  }

  @Override
  @Transactional(readOnly = false)
  public void autoHealProductDistributionTask(Product product) throws Exception {
    if (autoHealProductDistributionTask) {
      List<ProductDistributionTask> productDistributionTask = this.productDistributionTaskService
          .findStoreIdAndProductIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, product.getId());
      if (CollectionUtils.isEmpty(productDistributionTask)) {
        log.info("Auto heal product distribution task for productCode : {} ", product.getProductCode());
        List<ProductDistributionTask> productDistributionTasks = distributionTaskService
            .generateDistributionTaskForProduct(Constants.DEFAULT_STORE_ID, product.getCurrentVendor(),
                Collections.singletonList(product), product.getState());
        productDistributionTaskService.saveProductDistributionTaskList(productDistributionTasks);
        ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto = ConverterUtil
            .setProductDataAutoFixHistoryDto(product.getProductCode(), Constants.AUTOHEAL_DISTRIBUTION_TASK,
                StringUtils.EMPTY);
        kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, product.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDto));
      }
    }
  }

  @Override
  @Transactional
  public void productAndItemImagePathUpdate(ImagePathUpdateDomainEventModel request) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductCode()),
        ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    String productId = this.productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(request.getStoreId(),
        request.getProductCode());
    if (StringUtils.isNotEmpty(productId)) {
      List<String> productItemIds = this.productItemRepository.getIdsByProductIdAndMarkForDeleteFalse(productId);
      for (OldAndNewPathDomainEventModel oldAndNewPathDomainEventModel : request.getImageUpdatedPath()) {
        GdnPreconditions.checkArgument(StringUtils.isNotEmpty(oldAndNewPathDomainEventModel.getOldPath()),
            ErrorMessages.OLD_PATH_MUST_NOT_BE_EMPTY);
        GdnPreconditions.checkArgument(StringUtils.isNotEmpty(oldAndNewPathDomainEventModel.getNewPath()),
            ErrorMessages.NEW_PATH_MUST_NOT_BE_EMPTY);
        this.productImageRepository.updateLocationPathByProductId(oldAndNewPathDomainEventModel.getNewPath(), productId,
            oldAndNewPathDomainEventModel.getOldPath());
        if (CollectionUtils.isNotEmpty(productItemIds)) {
          this.productItemImageRepository.updateLocationPathByProductItem(oldAndNewPathDomainEventModel.getNewPath(),
              productItemIds, oldAndNewPathDomainEventModel.getOldPath());
        }
      }
    }
  }

  @Override
  public Page<Product> getAllProductDetailsWithMultipleFilter(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable,
      String storeId) throws Exception {
    return this.solrVendorCollectionService.getAllProductDetailsWithMultipleFilterSolr(
        distributionTaskMultipleFilterDTO, pageable, storeId);
  }

  @Override
  public Page<ProductBusinessPartnerMapper> getBusinessPartnerForVendor(String vendorId,
      Pageable pageable) {
    Page<ProductBusinessPartnerMapper> productBusinessPartnersForVendor =
        this.productRepository.findProductBusinessPartnerForVendor(vendorId, pageable);
    return productBusinessPartnersForVendor;
  }

  @Override
  @Transactional(readOnly = true)
  public Product getDetailsForAnyProductTypeByCode(String code) throws Exception {
    Product product = this.productRepository.findByProductCode(code);
    validateProductInfo(product, code);
    this.productUtils.initializeProductDetailsWithMFDTrue(product);
    return product;
  }

  @Override
  public Product getProductByCode(String code) {
    return this.productRepository.findByProductCode(code);
  }

  @Override
  public Product getProductByProductCodeAndMarkForDeleteFalse(String code) {
    return this.productRepository.findByProductCodeAndMarkForDeleteFalse(code);
  }

  @Override
  @Transactional
  public Product getDetailsForProductByProductCodeAndMarkForDeleteFalse(String code) {
    Product product = this.productRepository.findByProductCodeAndMarkForDeleteFalse(code);
    if (Objects.nonNull(product)) {
      this.productUtils.initializeAllProductDetails(product);
    }
    return product;
  }

  @Override
  @Transactional
  public Product getDetailsForProductByProductCode(String code) {
    Product product = this.productRepository.findByProductCode(code);
    if (Objects.nonNull(product)) {
      this.productUtils.initializeAllProductDetails(product);
    }
    return product;
  }

  @Override
  public Product findProductByProductCode(String code) {
    return this.productRepository.findByProductCode(code);
  }

  @Override
  @Transactional
  public Product getProductByProductCodeAndMarkForDeleteFalseAndItems(String code) {
    Product product = this.productRepository.findByProductCodeAndMarkForDeleteFalse(code);
    Hibernate.initialize(product.getProductItems());
    return product;
  }

  @Override
  public List<String> getProductCodeList(List<String> productCodes) throws Exception {
    return this.productRepository.findProductCodesProductCodesIn(productCodes);
  }

  @Override
  public NeedRevisionResponse doProductNeedForCorrection(String storeId, String requestId, String username, String vendorCode,
    NeedRevisionRequest request) {
    NeedRevisionDTO needRevisionDTO = new NeedRevisionDTO();
    int productsCountFailedToUpdateNeedCorrection = 0;
    for (String productCode : request.getProductCodes()) {
      try {
        needRevisionDTO = getProductService().doProductNeedForCorrectionSync(vendorCode, productCode, request);
          publishNeedCorrectionEvent(storeId, productCode);
      } catch (Exception e) {
        productsCountFailedToUpdateNeedCorrection++;
        log.error("Error while updating the need for correction. "
                + "Product could be in invalid state in PDT or PBP : {} . needRevisionType in request can be invalid = {}",
            productCode,
            request.getNeedRevisionType(),
            e);
      }
    }
    NeedRevisionResponse needRevisionResponse = new NeedRevisionResponse();
    needRevisionResponse.setFailedToUpdateNeedCorrectionProductsCount(productsCountFailedToUpdateNeedCorrection);
    if (productsCountFailedToUpdateNeedCorrection == 0) {
      setNeedRevisionResponse(request, needRevisionResponse);
      needRevisionResponse.setSuccess(true);
    } else {
      needRevisionResponse.setSuccess(false);
    }
    return needRevisionResponse;
  }

  private void publishNeedCorrectionEvent(String storeId, String productCode) throws Exception {
    kafkaProducer.send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, productCode,
        new PDTNeedRevisionEventModel(storeId, productCode));
    solrVendorCollectionService.deleteProductFromSolr(productCode);
  }

  private void setNeedRevisionResponse(NeedRevisionRequest request,
      NeedRevisionResponse needRevisionResponse) {
    if (NeedRevisionType.CONTENT.name().equals(request.getNeedRevisionType())) {
      needRevisionResponse.setContentNeedCorrection(true);
    } else if (NeedRevisionType.IMAGE.name().equals(request.getNeedRevisionType())) {
      needRevisionResponse.setImageNeedCorrection(true);
    } else {
      needRevisionResponse.setContentNeedCorrection(true);
      needRevisionResponse.setImageNeedCorrection(true);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public NeedRevisionDTO doProductNeedForCorrectionSync(String vendorCode, String productCode,
      NeedRevisionRequest request) throws Exception {
    Product product = getProductByProductCodeAndMarkForDeleteFalseAndItems(productCode);
    validateNeedRevisionRequest(vendorCode, request, product, productCode);
    setProductNotes(request, product);
    setProductItemNotes(request, product);
    boolean eligibleForNeedRevision = WorkflowState.IN_REVIEW.equals(product.getState());
    log.info("Need revision productCode : {}, current state : {}, needRevisionType : {}, eligible : {} ", productCode,
        product.getState().name(), request.getNeedRevisionType(), eligibleForNeedRevision);
    sendProductToNeedRevision(request, product);
    updateProductDistributionTask(product);
    return new NeedRevisionDTO(eligibleForNeedRevision, product.getState());
  }

  private void updateProductDistributionTask(Product product) {
    ProductDistributionTask productDistributionTask =
        this.productDistributionTaskService.findByProductId(product.getId());
    this.productDistributionTaskService.updateState(productDistributionTask, product.getState());
  }

  private void validateNeedRevisionRequest(String vendorCode, NeedRevisionRequest request, Product product,
      String productCode) throws NotFoundException {
    validateProductInfo(product, productCode);
    NeedRevisionType isNeedRevisionTypeValid=NeedRevisionType.valueOf(request.getNeedRevisionType());
    GdnPreconditions.checkState(WorkflowState.IN_REVIEW.equals(product.getState()),
        INVALID_PRODUCT_STATE_FOR_NEED_CORRECTION);
    GdnPreconditions.checkArgument(product.getCurrentVendor().getVendorCode().equals(vendorCode),
        PRODUCT_UNASSIGNED_TO_VENDOR_ERROR);
    GdnPreconditions
        .checkArgument(!StringUtils.equals(Constants.INTERNAL_BUSINESS_PARTNER, product.getBusinessPartnerName()),
            FLOW3_PRODUCT_ERROR);
  }

  private void sendProductToNeedRevision(NeedRevisionRequest request, Product product)
      throws Exception {
    setNeedRevisionState(request, product);
    product.setMarkForDelete(true);
    product.setAppealedProduct(false);
    getProductService().saveBulkProducts(Collections.singletonList(product));
  }

  private ProductService getProductService() {
    return applicationContext.getBean(ProductService.class);
  }

  private void setProductNotes(NeedRevisionRequest needRevisionRequest, Product product) throws IOException {
    if (Objects.nonNull(needRevisionRequest.getProductNotesRequest())) {
      ProductNotesResponse productNotesResponse = new ProductNotesResponse();
      if (StringUtils.isNotEmpty(product.getProductNotes())) {
        productNotesResponse = mapper.readValue(product.getProductNotes(), ProductNotesResponse.class);
      }
      if (NeedRevisionType.CONTENT.name().equals(needRevisionRequest.getNeedRevisionType())) {
        productNotesResponse.setVendorNotes(needRevisionRequest.getProductNotesRequest().getVendorNotes());
        productNotesResponse.setContentAdditionalNotes(
            needRevisionRequest.getProductNotesRequest().getContentAdditionalNotes());
        productNotesResponse.setVendorErrorFields(needRevisionRequest.getProductNotesRequest().getVendorErrorFields());
      } else if (NeedRevisionType.IMAGE.name().equals(needRevisionRequest.getNeedRevisionType())) {
        productNotesResponse.setImageReason(needRevisionRequest.getProductNotesRequest().getImageReason());
        productNotesResponse.setCommonImageReason(needRevisionRequest.getProductNotesRequest().getCommonImageReason());
        productNotesResponse.setAllVariants(needRevisionRequest.getProductNotesRequest().getAllVariants());
        productNotesResponse.setImagesAdditionalNotes(
            needRevisionRequest.getProductNotesRequest().getImagesAdditionalNotes());
      } else {
        BeanUtils.copyProperties(needRevisionRequest.getProductNotesRequest(), productNotesResponse);
      }
      log.info("set product notes productCode: {} , productNotes {}", product.getProductCode(),
          productNotesResponse);
      product.setProductNotes(mapper.writeValueAsString(productNotesResponse));
    }
  }

  private void setProductItemNotes(NeedRevisionRequest needRevisionRequest, Product product)
      throws JsonProcessingException {
    if (CollectionUtils.isNotEmpty(needRevisionRequest.getItemNotes())) {
      Map<String, ItemNotesRequest> itemNotesRequestMap = needRevisionRequest.getItemNotes().stream()
          .collect(Collectors.toMap(ItemNotesRequest::getSkuCode, itemNotesRequest -> itemNotesRequest, (a, b) -> a));
      for (ProductItem productItem : product.getProductItems()) {
        productItem.setItemNotes(mapper.writeValueAsString(itemNotesRequestMap.get(productItem.getSkuCode())));
      }
    }
  }

  private void setNeedRevisionState(NeedRevisionRequest request, Product product) {
    product.setState(WorkflowState.NEED_CORRECTION);
    if (NeedRevisionType.CONTENT.name().equals(request.getNeedRevisionType())) {
      product.setReviewType(ReviewType.CONTENT);
    } else if (NeedRevisionType.IMAGE.name().equals(request.getNeedRevisionType())) {
      product.setReviewType(ReviewType.IMAGE);
    } else {
      product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    }
  }

  private ScreeningProductBulkActionsRequest getScreeningProductBulkActionsRequest(String productCode,
      NeedRevisionRequest request) {
    ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    BeanUtils.copyProperties(request, screeningProductBulkActionsRequest, "productCodes");
    screeningProductBulkActionsRequest.setProductCodes(Collections.singletonList(productCode));
    return screeningProductBulkActionsRequest;
  }

  private ProductDTO getProductDTO(ProductListRequestDTO productListRequestDTO, String vendorId) {
    ProductDTO productDTO = new ProductDTO();
    BeanUtils.copyProperties(productListRequestDTO, productDTO, "workflowState");
    productDTO.setWorkflowState(productListRequestDTO.getWorkflowState());
    productDTO.setVendorId(vendorId);
    return productDTO;
  }

  @Override
  public List<Product> getProductListByProductCodes(List<String> productCodeList,
      List<WorkflowState> states) {
    return this.productRepository.findByProductCode(productCodeList, states);
  }

  @Override
  public String getvendorIdByVendorCode(String vendorCode) throws Exception {
    return this.vendorRepository.getVendorIdByVendorCode(vendorCode);
  }

  @Override
  public Map<String, Object> getProductStatusByVendor(String storeId, String vendorCode, Boolean postLive,
    Boolean edited, Boolean revised) throws IOException, SolrServerException {
    return solrVendorCollectionService.getFilterCounts(storeId, vendorCode, edited, postLive, revised);
  }

  @Override
  public Map<String, Object> getReviewConfigProductCountByVendor(String storeId,
      String vendorCode) throws Exception {
    try {
      return solrVendorCollectionService.getReviewConfigCountsByVendor(storeId, vendorCode);
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Error fetching counts from solr, fallback to db query, ", e);
      throw e;
    }
  }

  private WorkflowState getWorkflowStatePostApproval(Product product) throws Exception {
    return this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId());
  }

  @Override
  public Map<String, List<WorkflowState>> getWorkflowStatusForProducts(List<String> productIdList)
      throws Exception {
    List<ProductDistributionTask> productDistributionTaskList =
        this.productDistributionTaskRepository.getStatusForProducts(productIdList);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    for (ProductDistributionTask productDistributionTask : productDistributionTaskList) {
      if (!workflowStateMap.containsKey(productDistributionTask.getProductId())) {
        List<WorkflowState> workflowStateList = new ArrayList<>();
        workflowStateList.add(productDistributionTask.getState());
        workflowStateMap.put(productDistributionTask.getProductId(), workflowStateList);
      } else {
        List<WorkflowState> workflowStateList =
            workflowStateMap.get(productDistributionTask.getProductId());
        workflowStateList.add(productDistributionTask.getState());
        workflowStateMap.put(productDistributionTask.getProductId(), workflowStateList);
      }
    }
    return workflowStateMap;
  }

  @Override
  public Map<String, List<WorkflowState>> getWorkflowStatusForProducts(String vendorId,
      List<String> productIdList) throws Exception {
    if (CollectionUtils.isEmpty(productIdList) || StringUtils.isEmpty(vendorId)) {
      return new HashMap<>();
    }
    List<ProductDistributionTask> productDistributionTaskList =
        this.productDistributionTaskRepository.getStatusForProducts(vendorId, productIdList);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    for (ProductDistributionTask productDistributionTask : productDistributionTaskList) {
      if (!workflowStateMap.containsKey(productDistributionTask.getProductId())) {
        List<WorkflowState> workflowStateList = new ArrayList<>();
        workflowStateList.add(productDistributionTask.getState());
        workflowStateMap.put(productDistributionTask.getProductId(), workflowStateList);
      } else {
        List<WorkflowState> workflowStateList =
            workflowStateMap.get(productDistributionTask.getProductId());
        workflowStateList.add(productDistributionTask.getState());
        workflowStateMap.put(productDistributionTask.getProductId(), workflowStateList);
      }
    }
    return workflowStateMap;
  }

  private void isCurrentWorkflowEliglibleToReject(String productCode) throws Exception {
    ProductWorkflowStatusResponse productWorkflowStatusResponse =
        this.productBusinessPartnerService.getWorkflowStatus(productCode);
    String currentStates = productWorkflowStatusResponse.getStates().stream()
        .filter(state -> state.equals("ACTIVE")).findAny().orElse(null);
    if (!StringUtils.isEmpty(currentStates)) {
      throw new IllegalStateException("Cannot reject this product code " + productCode
          + " because product is have been activated before");
    }
  }

  private void isEliglibleToReject(ProductDistributionTask productDistributionTask,
    String vendorCode) throws Exception {
    Product product = productDistributionTask.getProduct();
    Vendor vendor = productDistributionTask.getVendor();
    GdnPreconditions.checkArgument(vendorCode.equals(product.getCurrentVendor().getVendorCode()),
            PRODUCT_REJECT_VENDOR_ERROR);
    GdnPreconditions.checkArgument(vendor.isAbleToReject(),
        "Vendor is not authorized to reject product");
    if (validateApprovedProductRejection && (product.isMarkForDelete())) {
      throw new ValidationException(ApiErrorCode.PRODUCT_NOT_FOUND.getCode(),
          ApiErrorCode.PRODUCT_NOT_FOUND.getDesc());
    }
    if (validateProductReject) {
      if (!product.isPostLive() && !product.isEdited()) {
        this.isCurrentWorkflowEliglibleToReject(product.getProductCode());
      }
    }
  }

  private void processRejectProduct(RejectProductDTO rejectProductDTO,
      ProductDistributionTask productDistributionTask) {
    try {
      Product product = productDistributionTask.getProduct();
      String username = GdnMandatoryRequestParameterUtil.getUsername();
      String approverAssignee = StringUtils.EMPTY;
      String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
      String requestId = GdnMandatoryRequestParameterUtil.getRequestId();
      String notes = mapper.writeValueAsString(ConverterUtil.getNotesAndRejectReason(rejectProductDTO));
      WorkflowState state = Enum.valueOf(WorkflowState.class, WorkflowState.REJECTED.toString());
      this.productBusinessPartnerService.deleteProductCollection(requestId, username, rejectProductDTO);
      log.info("product {} mark for delete in PBP, status : true successfully updated, invoking same for PDT",
          product.getProductCode());
      this.updateProductAndProductDistributionState(productDistributionTask, state);
      if (rejectProductDTO.isBulkAction()) {
        approverAssignee = username;
        notes = Constants.BULK_REJECTION.concat(notes);
      }
      this.removeProductDetailsAndProductDistributionTask(product, state, approverAssignee);
      if (pdtHistoryUpdateThroughEvent) {
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, product.getProductCode(),
            taskHistoryService.generatePDTHistoryEventModel(product.getStoreId(), GdnMandatoryRequestParameterUtil.getUsername(), product,
                product.getCurrentVendor(), notes, product.getState(), productDistributionTask.getTaskCode()));
        kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(),
          product.getProductCode(),
          ConverterUtil.convertToInternalHistoryModel(product.getStoreId(),
            product.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(),
            state.getDesc(), notes));
      } else {
        this.taskHistoryService.createTaskHistory(storeId, username, product, productDistributionTask.getVendor(), notes, state,
            productDistributionTask.getTaskCode());
      }
    } catch (Exception e) {
      log.error("Error while rejecting product rejectProductDTO : {}, productDistributionTask : {} ", rejectProductDTO,
          productDistributionTask, e);
      kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.ADD_PRODUCT_TO_PDT_RETRY,
          rejectProductDTO.getProductCode(),
          new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, rejectProductDTO.getProductCode(),
              Constants.PDT_RETRY_DELETE, StringUtils.EMPTY));
    }
  }

  private void productFinalApprovalByVendor(Product product) throws Exception {
    this.vendorQuotaCounterRepository.decrementInProgressQuota(product.getCurrentVendor(), 1);
  }

  @Override
  @Transactional
  public void rejectAndDiscardProduct(String productId, boolean isExceededSLA) throws Exception {
    log.info("Reject and cancel product {}", productId);
    Product product = this.productRepository.findByIdAndMarkForDeleteFalse(productId);
    if (Objects.nonNull(product) && product.getState() != WorkflowState.REJECTED) {
      if (isExceededSLA) {
        this.productRepository.updateWorkflowState(productId, WorkflowState.EXCEEDED_SLA);
      } else {
        this.productRepository.updateWorkflowState(productId, WorkflowState.REJECTED);
      }
      this.productBusinessPartnerService.republishToPDT(UUID.randomUUID().toString(), "System",
          product.getProductCode());
    } else {
      log.error("productId {} Cannot reject product if it's already REJECTED", productId);
      throw new IllegalArgumentException("Cannot reject product if it's already REJECTED");
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void rejectProduct(RejectProductDTO rejectProductDTO) throws Exception {
    ProductDistributionTask productDistributionTask = this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
            rejectProductDTO.getProductCode());
    if (productDistributionTask != null) {
      this.processRejectProduct(rejectProductDTO, productDistributionTask);
    } else {
      throw new IllegalArgumentException(
          "Product " + rejectProductDTO.getProductCode() + " is not found in distribution list!");
    }
  }

  @Override
  @Transactional
  public void rejectProductByVendor(RejectProductDTO rejectProductDTO, String vendorCode)
      throws Exception {
    ProductDistributionTask productDistributionTask = this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
            rejectProductDTO.getProductCode());
    if (productDistributionTask != null) {
      checkDistributionMappingStatus(productDistributionTask.getProduct());
      if (warehouseStockCheckSwitch) {
        checkStockAvailability(rejectProductDTO, productDistributionTask);
      }
      this.isEliglibleToReject(productDistributionTask, vendorCode);
      this.processRejectProduct(rejectProductDTO, productDistributionTask);
    } else {
      throw new ValidationException(ApiErrorCode.PRODUCT_NOT_FOUND.getCode(),
          ApiErrorCode.PRODUCT_NOT_FOUND.getDesc());
    }
  }

  private void checkStockAvailability(RejectProductDTO rejectProductDTO,
      ProductDistributionTask productDistributionTask) {
    if (!Constants.INTERNAL_BUSINESS_PARTNER.equals(productDistributionTask.getProduct().getBusinessPartnerCode())) {
      if (StringUtils.isBlank(rejectProductDTO.getMerchantCommissionType()) && (rejectProductDTO.isBulkAction()
          || rejectProductDTO.isSchedulerAction())) {
        GdnRestSingleResponse<ProfileResponse> profileResponse =
            xbpFeign.filterByBusinessPartnerCode(GdnMandatoryRequestParameterUtil.getStoreId(),
                Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
                productDistributionTask.getProduct().getBusinessPartnerCode());
        if (!profileResponse.isSuccess()) {
          log.error("XBP call failed for sellerCode: {} message: {} ",
              productDistributionTask.getProduct().getBusinessPartnerCode(), profileResponse.getErrorMessage());
          throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE, profileResponse.getErrorMessage());
        }
        Optional.ofNullable(profileResponse.getValue()).map(ProfileResponse::getCompany)
            .map(CompanyDTO::getMerchantType).ifPresent(rejectProductDTO::setMerchantCommissionType);
      }
    }
    if (Constants.INTERNAL_BUSINESS_PARTNER.equals(productDistributionTask.getProduct().getBusinessPartnerCode())
        || Arrays.asList(warehouseMerchantCommissionTypeList.split(Constants.COMMA))
        .contains(rejectProductDTO.getMerchantCommissionType())) {
      GdnRestSingleResponse<L2StockDetailResponse> response;
      List<String> skuCodes = productDistributionTask.getProduct().getProductItems().stream()
          .map(ProductItem::getSkuCode).toList();
      for (String skuCode : skuCodes) {
        response = xInventoryFeign.getStockDetailsByWarehouseItemSku(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), skuCode);
        if (!response.isSuccess()) {
          log.error(INVENTORY_ERROR_MESSAGE, response.getErrorMessage());
          throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
              response.getErrorMessage());
        }
        if (response.getValue().isDistributionWarehouseAvailable() || response.getValue()
            .isNonDistributionWarehouseAvailable()) {
          log.error("Warehouse stock exists for this product L2 {}", skuCode);
          throw new ValidationException(
              ApiErrorCode.PRODUCT_CANNOT_BE_REJECTED_BECAUSE_WAREHOUSE_STOCK_EXISTS.getCode(),
              ApiErrorCode.PRODUCT_CANNOT_BE_REJECTED_BECAUSE_WAREHOUSE_STOCK_EXISTS.getDesc());
        }
      }
    }
  }

  private void checkDistributionMappingStatus(Product product) {
    if (product.getDistributionMappingStatus() > Constants.ZERO) {
      log.error("Distribution mapping exists for {}", product.getProductCode());
      throw new ValidationException(
        ApiErrorCode.REJECTION_OF_DISTRIBUTION_PRODUCT_IS_NOT_ALLOWED.getCode(),
        ApiErrorCode.REJECTION_OF_DISTRIBUTION_PRODUCT_IS_NOT_ALLOWED.getDesc());
    }
  }

  private void removeProductDetailsAndProductDistributionTask(Product product, WorkflowState state,
      String approverAssignee)
      throws Exception {
    this.removeProductWithMarkForDelete(product, approverAssignee);
    this.productDistributionTaskService.removeProductDistributionTask(product.getId(),
        state.toString());
  }

  @Override
  @Transactional
  public void removeProductWithMarkForDelete(Product product, String approverAssignee) throws Exception {
    try {
      product.setMarkForDelete(true);
      product.setRevised(false);
      product.setProductNotes(null);
      product.setAppealedProduct(false);
      this.productRepository.save(product);
      this.productItemRepository.deleteByProductId(product.getId(),
          GdnMandatoryRequestParameterUtil.getUsername());
      this.productAttributeRepository.deleteByProductId(product.getId(),
          GdnMandatoryRequestParameterUtil.getUsername());
      this.productImageRepository.deleteByProductId(product.getId(),
          GdnMandatoryRequestParameterUtil.getUsername());
      this.productItemImageRepository.deleteByProductId(product.getId(),
          GdnMandatoryRequestParameterUtil.getUsername());
      this.productReviewerService.markForDeleteByProductCode(product.getProductCode(), approverAssignee);
    } catch (Exception e) {
      log.error("error while rejecting product with markForDelete. productCode: {}",
          product.getProductCode(), e);
      throw e;
    }
  }

  @Override
  @Transactional
  public Product replaceProduct(Product newProduct) {
    log.info("Replace dicarded Product Code {}", newProduct.getProductCode());
    Product oldProduct = this.productRepository.findByProductCode(newProduct.getProductCode());
    if (oldProduct != null && ALLOW_REPLACE_PRODUCT_DATA.contains(oldProduct.getState())) {
      this.clearProductDetails(oldProduct);
      this.productUtils.regenerateProductReplacementDetails(oldProduct, newProduct);
      return this.productRepository.saveAndFlush(oldProduct);
    } else {
      log.error("Product Code {} is not exist !!", newProduct.getProductCode());
      throw new IllegalArgumentException(
          "Product Code " + newProduct.getProductCode() + " is not exist !!");
    }
  }

  @Override
  @Transactional
  public void saveBulkProducts(List<Product> productList) throws Exception {
    this.productRepository.saveAll(productList);
  }
  private void sortProductImagesBySequenceId(Product product) {
    if (null != product && (!CollectionUtils.isEmpty(product.getProductImages()))) {
      Collections.sort(product.getProductImages(), new Comparator<ProductImage>() {
        @Override
        public int compare(ProductImage productImage1, ProductImage productImage2) {
          return productImage1.getSequence().compareTo(productImage2.getSequence());
        }
      });
    }
  }

  private void sortProductItemImages(Product product) {
    if (null != product && (!CollectionUtils.isEmpty(product.getProductItems()))) {
      List<ProductItem> productItemList = product.getProductItems();
      for (ProductItem productItem : productItemList)
        sortProductItemImagesBySequenceId(productItem);
    }
  }

  private void sortProductItemImagesBySequenceId(ProductItem productItem) {
    if (null != productItem && (!CollectionUtils.isEmpty(productItem.getProductItemImages()))) {
      Collections.sort(productItem.getProductItemImages(), new Comparator<ProductItemImage>() {
        @Override
        public int compare(ProductItemImage productItemImage1, ProductItemImage productItemImage2) {
          return productItemImage1.getSequence().compareTo(productItemImage2.getSequence());
        }
      });
    }
  }

  @Override
  @Transactional
  public Product update(Product product) {
    if ((product.getId() == null) || (this.productRepository.findById(product.getId()).isEmpty())) {
      throw new IllegalArgumentException(
          "can not update un existence data with id : " + product.getId());
    }
    return this.productRepository.saveAndFlush(product);
  }

  @Override
  @Transactional
  public void updateProduct(Product product) throws Exception {
    if (product == null) {
      log.error("error updating Product. product is null");
      return;
    }
    try {
      this.productRepository.save(product);
    } catch (Exception e) {
      log.error("error updating Product. productCode: {}", product.getProductCode(), e);
    }
  }

  private void updateProductAndProductDistributionState(
      ProductDistributionTask productDistributionTask, WorkflowState state) {
    this.productDistributionTaskService.updateState(productDistributionTask, state);
    this.updateState(productDistributionTask.getProduct(), state);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Product updateProductDetails(Product existingProduct, Product newProduct)
      throws Exception {

    ProductSystemParameterResponse systemParameterResponse = findSystemParameter(Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    if (Boolean.valueOf(systemParameterResponse.getValue())) {
      if (Objects.nonNull(existingProduct.getVideoUrl()) && !existingProduct.getVideoUrl()
          .equals(newProduct.getVideoUrl()) && StringUtils.isNotBlank(newProduct.getVideoUrl())) {
        boolean youTubeUrlResponse = productUtils.validateYouTubeUrl(newProduct.getVideoUrl(), youTube);
        if (!youTubeUrlResponse) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_URL);
        }
      }
    }
    String brandCode = StringUtils.isNotBlank(newProduct.getBrandCode()) ?
        newProduct.getBrandCode() :
        productUtils.getBrandCodeByBrandName(newProduct.getBrand());
    if (!productUtils.validateProtectedBrand(brandCode, existingProduct.getBusinessPartnerCode())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, PROTECTED_BRAND);
    }
    newProduct.setRestrictedKeywordsDetected(existingProduct.getRestrictedKeywordsDetected());
    newProduct.setForceReview(existingProduct.isForceReview());
    List<ProductHistoryDTO> productHistoryDTOList =
        productUtils.getProductDetailChanges(existingProduct, newProduct);
    Product product = this.productUtils.replaceProductDetails(existingProduct, newProduct, true);
    product = this.productRepository.save(product);
    if (CollectionUtils.isNotEmpty(productHistoryDTOList)) {
      if (pdtHistoryUpdateThroughEvent) {
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, GdnMandatoryRequestParameterUtil.getUsername(), taskHistoryService
            .generatePDTHistoryEventModel(product.getStoreId(), product.getUpdatedBy(), product,
                product.getCurrentVendor(),
                String.format(Constants.UPDATE_HISTORY_FORMAT, productUtils.toJson(productHistoryDTOList)),
                product.getState(), productDistributionTaskRepository.getTaskCodeForProduct(product.getId())));
      } else {
        saveProductHistory(product, productHistoryDTOList);
      }
    }
    return product;
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Product updateProductDetails(Product existingProduct, Product newProduct, boolean deleteOriginalImages)
      throws Exception {

    ProductSystemParameterResponse systemParameterResponse = findSystemParameter(Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    if (Boolean.valueOf(systemParameterResponse.getValue())) {
      if (Objects.nonNull(existingProduct.getVideoUrl()) && !existingProduct.getVideoUrl()
          .equals(newProduct.getVideoUrl()) && StringUtils.isNotBlank(newProduct.getVideoUrl())) {
        boolean youTubeUrlResponse = productUtils.validateYouTubeUrl(newProduct.getVideoUrl(), youTube);
        if (!youTubeUrlResponse) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_URL);
        }
      }
    }
    String brandCode;
    if (brandExistCheckEnabled) {
      brandCode = productUtils.getBrandCodeByBrandName(newProduct.getBrand());
    } else {
      brandCode = StringUtils.isNotBlank(newProduct.getBrandCode()) ?
          newProduct.getBrandCode() :
          productUtils.getBrandCodeByBrandName(newProduct.getBrand());
    }
    if (StringUtils.isEmpty(brandCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, BRAND_DOES_NOT_EXIST);
    }
    if (!productUtils.validateProtectedBrand(brandCode, existingProduct.getBusinessPartnerCode())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, PROTECTED_BRAND);
    }
    setDgLevelDefaultValueIfNull(existingProduct, newProduct);
    newProduct.setRestrictedKeywordsDetected(existingProduct.getRestrictedKeywordsDetected());
    newProduct.setForceReview(existingProduct.isForceReview());
    List<ProductHistoryDTO> productHistoryDTOList =
        productUtils.getProductDetailChanges(existingProduct, newProduct);
    List<ProductHistoryDTO> imageChanges = productUtils.getImageChanges(existingProduct, newProduct);
    productHistoryDTOList.addAll(imageChanges);
    fileStorageService.validateProductImages(newProduct);
    if (deleteOriginalImages && !existingProduct.isEdited()) {
      publishDeleteOriginalImagesForProductAndItemsEvent(existingProduct);
    }
    Product product = this.productUtils.replaceProductDetails(existingProduct, newProduct, true);
    product = this.productUtils.replaceProductImageDetails(product, newProduct);
    this.productUtils.regenerateProductImageDetails(product);
    product.setAppealedProduct(false);
    product = this.productRepository.save(product);
    if (CollectionUtils.isNotEmpty(productHistoryDTOList)) {
      if (pdtHistoryUpdateThroughEvent) {
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, product.getProductCode(), taskHistoryService
            .generatePDTHistoryEventModel(product.getStoreId(),  GdnMandatoryRequestParameterUtil.getUsername(), product,
                product.getCurrentVendor(),
                String.format(Constants.UPDATE_HISTORY_FORMAT, productUtils.toJson(productHistoryDTOList)),
                product.getState(), productDistributionTaskRepository.getTaskCodeForProduct(product.getId())));
      } else {
        saveProductHistory(product, productHistoryDTOList);
      }
    }
    return product;
  }

  private void setDgLevelDefaultValueIfNull(Product existingProduct, Product newProduct) {
    existingProduct.getProductItems().stream()
        .filter(productItem -> Objects.isNull(productItem.getDangerousGoodsLevel()))
        .forEach(productItem -> productItem.setDangerousGoodsLevel(0));
    newProduct.getProductItems().stream().filter(productItem -> Objects.isNull(productItem.getDangerousGoodsLevel()))
        .forEach(productItem -> productItem.setDangerousGoodsLevel(0));
  }

  private void publishDeleteOriginalImagesForProductAndItemsEvent(Product existingProduct) {
    log.info("Publishing delete original images for product and items event for product code : {} ",
        existingProduct.getProductCode());
    kafkaProducer.send(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT,existingProduct.getProductCode(),existingProduct);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Product updateEditedProductDetails(Product existingProduct, Product newProduct, List<String> modifiedFields)
      throws Exception {
    ProductSystemParameterResponse systemParameterResponse = findSystemParameter(Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    if (Boolean.valueOf(systemParameterResponse.getValue())) {
      if (Objects.nonNull(existingProduct.getVideoUrl()) && !existingProduct.getVideoUrl()
          .equals(newProduct.getVideoUrl()) && StringUtils.isNotBlank(newProduct.getVideoUrl())) {
        boolean youTubeUrlResponse = productUtils.validateYouTubeUrl(newProduct.getVideoUrl(), youTube);
        if (!youTubeUrlResponse) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_URL);
        }
      }
    }
    Product product = this.productUtils.replaceProductDetails(existingProduct, newProduct, true);
    if (refreshProductImageDetails) {
      productUtils.replaceProductImageDetails(product, newProduct);
    }
    updateProductNotesForEditedProducts(product, modifiedFields);
    product.setState(newProduct.getState());
    product.setReviewType(newProduct.getReviewType());
    product.setEdited(true);
    product.setForceReview(newProduct.isForceReview());
    ImageUtils.setActiveFlagInProductAndItemImages(product);
    product = this.productRepository.save(product);
    return product;
  }

  @Override
  public void updateProductNotesForEditedProducts(Product product, List<String> modifiedFields)
      throws IOException {
    ProductNotesResponse productNotesResponse = new ProductNotesResponse();
    if (Objects.nonNull(product.getProductNotes())) {
      productNotesResponse = mapper.readValue(product.getProductNotes(), ProductNotesResponse.class);
    }
    List<String> existingModifiedFields = Optional.ofNullable(productNotesResponse.getModifiedFields()).orElse(new ArrayList<>());
    existingModifiedFields.addAll(Optional.ofNullable(modifiedFields).orElse(new ArrayList<>()).stream()
        .filter(modifiedField -> !existingModifiedFields.contains(modifiedField))
        .collect(Collectors.toList()));
    productNotesResponse.setModifiedFields(existingModifiedFields);
    productNotesResponse.setLastModified(Constants.EDITED);
    product.setProductNotes(mapper.writeValueAsString(productNotesResponse));
  }

  private ProductSystemParameterResponse findSystemParameter(String variable) {
    GdnRestSingleResponse response = pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(), variable);
    if (!response.isSuccess()) {
      log.error(PBP_ERROR_MESSAGE, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return (ProductSystemParameterResponse) response.getValue();
  }

  private void saveProductHistory(Product product, List<ProductHistoryDTO> productHistoryDTOList)
      throws Exception {
    TaskHistory taskHistory =
        new TaskHistory(product.getProductCode(), product.getProductName(), product.getCategoryCode(),
            product.getCategoryName(), product.getCurrentVendor(),
            String.format(Constants.UPDATE_HISTORY_FORMAT, productUtils.toJson(productHistoryDTOList)),
            product.getState(), product.getStoreId(), product.getUpdatedBy(),
            productDistributionTaskRepository.getTaskCodeForProduct(product.getId()));
    taskHistoryRepository.saveAndFlush(taskHistory);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Product updateProductImageDetails(Product existingProduct, Product newProduct, boolean deleteOriginalImages)
      throws Exception {
    //Todo check if validation is needed or not
    fileStorageService.updateNewlyAddedImagePath(newProduct, existingProduct);
    final List<ProductHistoryDTO> imageChanges = productUtils.getImageChanges(existingProduct, newProduct);
    fileStorageService.validateProductImages(newProduct);
    if(deleteOriginalImages && !existingProduct.isEdited()){
      this.deleteOriginalImagesForProductAndItems(existingProduct);
    }
    newProduct.setRestrictedKeywordsDetected(existingProduct.getRestrictedKeywordsDetected());
    Product product = this.productUtils.replaceProductImageDetails(existingProduct, newProduct);
    this.productUtils.regenerateProductImageDetails(product);
    product = this.productRepository.save(product);
    if (CollectionUtils.isNotEmpty(imageChanges)) {
      if (pdtHistoryUpdateThroughEvent) {
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, product.getProductCode(), taskHistoryService
            .generatePDTHistoryEventModel(product.getStoreId(),  GdnMandatoryRequestParameterUtil.getUsername(), product,
                product.getCurrentVendor(),
                String.format(Constants.UPDATE_HISTORY_FORMAT, productUtils.toJson(imageChanges)), product.getState(),
                productDistributionTaskRepository.getTaskCodeForProduct(product.getId())));
      } else {
        saveProductHistory(product, imageChanges);
      }
    }
    return product;
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Product updateEditedProductImageDetails(Product existingProduct, Product newProduct)
      throws Exception {
    Product product = this.productUtils.replaceProductImageAndProductItemImages(existingProduct, newProduct);
    this.productUtils.regenerateProductImageDetails(product);
    product.setState(newProduct.getState());
    product.setReviewType(newProduct.getReviewType());
    product.setEdited(true);
    product.setImageViolations(newProduct.getImageViolations());
    product.setProductPredictionScore(newProduct.getProductPredictionScore());
    Map<String, ProductItem> updatedProductItemMap = newProduct.getProductItems().stream()
        .collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity(), (a, b) -> a));
    product.getProductItems().forEach(
        productItem -> ProductDomainEventModelConverterUtils.setPriceInfoForImageEdit(productItem,
            updatedProductItemMap));
    existingProduct.setForceReview(newProduct.isForceReview());
    ImageUtils.setActiveFlagInProductAndItemImages(product);
    fileStorageService.validateProductImages(product);
    product = this.productRepository.save(product);
    return product;
  }

  @Override
  @Transactional
  public Product updateState(Product product, WorkflowState state) {
    product.setState(state);
    return this.update(product);
  }

  @Override
  public Product updateStateAndRemoveAssigneeDetails(Product product, WorkflowState state) {
    product.setState(state);
    return this.update(product);
  }

  @Override
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(String storeId,
      String requestId, PrimaryFilterDTO request, int page, int size) throws IOException, SolrServerException {
    List<WorkflowState> state = getWorkFlowState();
    List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList=
        solrVendorCollectionService.getBusinessPartnerList(storeId, request, state, page, size);
    return new GdnRestListResponse<>(productBusinessPartnerMapperResponseList,
        new PageMetaData(page, size, productBusinessPartnerMapperResponseList.size()), requestId);
  }

  @Override
  public List<String> getAssigneeList(String storeId, String requestId, PrimaryFilterDTO request) {
    List<WorkflowState> state = getWorkFlowState();
    Date[] startAndEndDates = getStartAndEndDate(request.getTimeFilterType());
    List<String> assigneeEmailIdList = new ArrayList<>();
    if(Boolean.FALSE.equals(request.getAssignment())) {
      assigneeEmailIdList.add(EMPTY_DROPDOWN_LIST_ELEMENT);
      return assigneeEmailIdList;
    }
    if (Objects.nonNull(request.getContentPending()) && Objects.nonNull(request.getImagePending())) {
      if (request.getContentPending().equals(Boolean.FALSE) && request.getImagePending().equals(Boolean.TRUE)) {
        assigneeEmailIdList = this.productRepository
            .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateImagePending(storeId, request.getKeyword(), state,
                startAndEndDates[0], startAndEndDates[1], request.getAssignment(), request.getVendorCode(),
                request.getBrandPending());
      } else if (request.getContentPending().equals(Boolean.TRUE) && request.getImagePending().equals(Boolean.FALSE)) {
        assigneeEmailIdList = this.productRepository
            .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentPending(storeId, request.getKeyword(), state,
                startAndEndDates[0], startAndEndDates[1], request.getAssignment(), request.getVendorCode(),
                request.getBrandPending());
      }
      else {
        assigneeEmailIdList = this.productRepository
            .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(storeId, request.getKeyword(),
                state, startAndEndDates[0], startAndEndDates[1], request.getAssignment(), request.getVendorCode(),
                request.getBrandPending());
      }
    } else {
      assigneeEmailIdList = this.productRepository
          .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(storeId, request.getKeyword(),
              state, startAndEndDates[0], startAndEndDates[1], request.getAssignment(), request.getVendorCode(),
              request.getBrandPending());
    }
    assigneeEmailIdList = assigneeEmailIdList.stream().filter(
        assigneeEmailId -> (StringUtils.isEmpty(request.getKeyword()) || assigneeEmailId
            .contains(request.getKeyword()))).distinct().collect(Collectors.toList());
    if(CollectionUtils.isEmpty(assigneeEmailIdList)) {
      assigneeEmailIdList.add(EMPTY_DROPDOWN_LIST_ELEMENT);
    }
    return assigneeEmailIdList;
  }

  @Override
  public Page<ProductAndReviewerDetailsDTO> getProductList(String storeId, String requestId, String username,
      SummaryFilterDTO summaryFilterDTO, int page, int size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    List<WorkflowState> states = getWorkFlowState();
    try {
      Page<ProductAndReviewerDetailsDTO> productPage = this.solrVendorCollectionService
          .getVendorProductsList(storeId, summaryFilterDTO, states, pageable);
      setVendorForProductList(
        productPage.getContent().stream().map(ProductAndReviewerDetailsDTO::getProduct)
          .collect(Collectors.toList()));
      setProductReviewerDetails(storeId,
          productPage.getContent().stream().map(ProductAndReviewerDetailsDTO::getProductReviewer)
              .collect(Collectors.toList()));
      return productPage;
    } catch (Exception e) {
      log.error("Error fetching vendor product list from solr for request : {}, ", summaryFilterDTO,
          e);
      throw e;
    }
  }

  @Override
  public void setVendorForProductList(List<Product> productList) {
    Map<String, Vendor> vendorCodeAndVendorMap = new HashMap<>();
    for (Product product : productList) {
      if (Objects.nonNull(product.getCurrentVendor())) {
        String vendorCode = product.getCurrentVendor().getVendorCode();
        if (!vendorCodeAndVendorMap.containsKey(vendorCode)) {
          vendorCodeAndVendorMap
              .put(vendorCode, this.vendorRepository.findByVendorCodeAndMarkForDeleteFalse(vendorCode));
        }
        product.setCurrentVendor(vendorCodeAndVendorMap.get(vendorCode));
      }
    }
  }

  private void setProductReviewerDetails(String storeId, List<ProductReviewer> productReviewerList) {
    Map<String, ProductReviewer> productReviewerMap = productReviewerService.findProductReviewerMapByProductCodes(storeId,
        productReviewerList.stream().map(ProductReviewer::getProductCode).collect(Collectors.toList()));
    for (ProductReviewer productReviewer : productReviewerList) {
      if (productReviewerMap.containsKey(productReviewer.getProductCode())) {
        ProductReviewer productReviewerDetails = productReviewerMap.get(productReviewer.getProductCode());
        productReviewer.setApproverAssignee(productReviewerDetails.getApproverAssignee());
        productReviewer.setAssignedDate(productReviewerDetails.getAssignedDate());
        productReviewer.setApprovedDate(productReviewerDetails.getApprovedDate());
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public List<String> doVendorProductAction(String storeId, List<String> productCodes, String assignedBy, String action,
      String assignedTo, Date date) throws Exception {
    if (UNASSIGN.equals(action)) {
      return assignReviewersForProducts(storeId, productCodes, null, null, assignedBy);
    } else {
      return assignReviewersForProducts(storeId, productCodes, date, assignedTo, assignedBy);
    }
  }

  @Override
  public void bulkVendorProductAction(String storeId, Date date,
      BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO) {
    List<ProductReviewer> productReviewers = productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(storeId,
        bulkScreeningProductActionsDTO.getProductCodes());
    for (ProductReviewer productReviewer : productReviewers) {
      productReviewer.setApproverAssignee(bulkScreeningProductActionsDTO.getAssignTo());
      productReviewer.setAssignedDate(new Date());
    }
    productReviewerService.saveAll(productReviewers);
    List<Product> productList = getProductsByProductCodes(bulkScreeningProductActionsDTO.getProductCodes());
    if (pdtHistoryUpdateThroughEvent) {
      String reason = null;
      reason = getProductActionReason(bulkScreeningProductActionsDTO.getAssignTo(),
          bulkScreeningProductActionsDTO.getAssignedBy());
      String finalReason = reason;
      productList.forEach(product -> {
        kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT, product.getProductCode(), taskHistoryService
            .generatePDTHistoryEventModel(product.getStoreId(), GdnMandatoryRequestParameterUtil.getUsername(), product,
                product.getCurrentVendor(), finalReason, product.getState(),
                productDistributionTaskRepository.getTaskCodeForProduct(product.getId())));
      });
    } else {
      bulkSaveTaskHistory(storeId, bulkScreeningProductActionsDTO.getAssignTo(),
          bulkScreeningProductActionsDTO.getAssignedBy(), productList);
    }
  }

  private List<String> assignReviewersForProducts(String storeId, List<String> productCodes,
      Date assignedDate, String assignedTo, String assignedBy) throws Exception {
    List<ProductReviewer> productReviewers =
        productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(storeId, productCodes);
    for (ProductReviewer productReviewer : Optional.ofNullable(productReviewers).orElse(new ArrayList<>())) {
      String oldAssigneeName = productReviewer.getApproverAssignee();
      if (StringUtils.isBlank(assignedTo) && StringUtils.isBlank(oldAssigneeName)) {
        log.warn("Both assignedTo and oldAssigneeName are blank for product code: {} ",
            productReviewer.getProductCode());
        continue;
      }
      productReviewer.setAssignedDate(assignedDate);
      productReviewer.setApproverAssignee(assignedTo);
      productReviewerService.save(productReviewer);
      Product product = productRepository.findByProductCodeAndMarkForDeleteFalse(productReviewer.getProductCode());
      if (Objects.nonNull(product)) {
        if (pdtHistoryUpdateThroughEvent) {
          TaskHistory taskHistory = null;
          taskHistory = getTaskHistory(product, assignedTo, oldAssigneeName, taskHistory);
          kafkaProducer.send(DomainEventName.PDT_PRODUCT_HISTORY_EVENT,
              taskHistoryService.generatePDTHistoryEventModel(product.getStoreId(), GdnMandatoryRequestParameterUtil.getUsername(), product,
                  product.getCurrentVendor(), taskHistory.getReason(), product.getState(), taskHistory.getTaskCode()));
        } else {
          saveTaskHistory(product, assignedTo, assignedBy, storeId, oldAssigneeName);
        }
      }
    }
    return Optional.ofNullable(productReviewers).orElse(new ArrayList<>()).stream()
        .map(ProductReviewer::getProductCode).collect(Collectors.toList());
  }

  private void saveTaskHistory(Product product, String assignedTo, String assignedBy, String storeId, String oldAssigneeName)
      throws JsonProcessingException {
    TaskHistory taskHistory = null;
    taskHistory = getTaskHistory(product, assignedTo, oldAssigneeName, taskHistory);
    if (Objects.nonNull(taskHistory)) {
      this.taskHistoryRepository.save(taskHistory);
    }
  }

  private TaskHistory getTaskHistory(Product product, String assignedTo, String oldAssigneeName,
      TaskHistory taskHistory) throws JsonProcessingException {
    List<ProductHistoryDTO> productHistoryDTOList = new ArrayList<>();
    if (StringUtils.isNotBlank(oldAssigneeName) && StringUtils.isBlank(assignedTo)) {
      productHistoryDTOList.add(new ProductHistoryDTO(null, ASSIGNEE, oldAssigneeName, StringUtils.EMPTY));
      taskHistory = getTaskHistoryByName(product, productHistoryDTOList, Constants.UNASSIGNED_HISTORY_FORMAT);
    } else if (StringUtils.isNotBlank(assignedTo) && StringUtils.isBlank(oldAssigneeName)) {
      productHistoryDTOList.add(new ProductHistoryDTO(null, ASSIGNEE, StringUtils.EMPTY, assignedTo));
      taskHistory = getTaskHistoryByName(product, productHistoryDTOList, Constants.ASSIGNED_HISTORY_FORMAT);
    } else if (StringUtils.isNotBlank(oldAssigneeName) && StringUtils.isNotBlank(assignedTo)) {
      productHistoryDTOList.add(new ProductHistoryDTO(null, ASSIGNEE, oldAssigneeName, assignedTo));
      taskHistory = getTaskHistoryByName(product, productHistoryDTOList, Constants.UPDATE_HISTORY_FORMAT);
    }
    return taskHistory;
  }

  private TaskHistory getTaskHistoryByName(Product product, List<ProductHistoryDTO> productHistoryDTOList, String reason)
      throws JsonProcessingException {
    TaskHistory taskHistory;
    taskHistory = new TaskHistory(product.getProductCode(), product.getProductName(), product.getCategoryCode(),
        product.getCategoryName(), product.getCurrentVendor(),
        String.format(reason, productUtils.toJson(productHistoryDTOList)),
        product.getState(), product.getStoreId(), product.getUpdatedBy(),
        productDistributionTaskRepository.getTaskCodeForProduct(product.getId()));
    return taskHistory;
  }

  private void bulkSaveTaskHistory(String storeId, String assignedTo, String assignedBy, List<Product> productList) {
    String reason = null;
    reason = getProductActionReason(assignedTo, assignedBy);

    List<TaskHistory> taskHistoryList = new ArrayList<>();
    for (Product product : productList) {
      TaskHistory taskHistory =
          new TaskHistory(product.getProductCode(), product.getProductName(), product.getCategoryCode(),
              product.getCategoryName(), product.getCurrentVendor(), reason, product.getState(), storeId,
              product.getUpdatedBy(), productDistributionTaskRepository.getTaskCodeForProduct(product.getId()));
      taskHistoryList.add(taskHistory);
    }
    this.taskHistoryRepository.saveAll(taskHistoryList);
  }

  private String getProductActionReason(String assignedTo, String assignedBy) {
    String reason;
    if (Objects.isNull(assignedTo)) {
      reason = new StringBuilder(UNASSIGNED_REASON).append(assignedBy).toString();
    } else {
      reason = new StringBuilder(ASSIGNED_REASON).append(assignedTo).append(ASSIGNED_BY).append(assignedBy).toString();
    }
    return reason;
  }

  @Transactional(rollbackFor = Exception.class)
  @Override
  public ApproveProductResponseDto updateAndApproveProduct(String vendorCode, Product newProduct,
    String notes, boolean isQuickApproval, ProductReviewer productReviewer) throws Exception {
    ApproveProductResponseDto responseDto;
    Product existingProduct = getAllProductDetailsByCode(newProduct.getProductCode());
    fileStorageService.updateNewlyAddedImagePath(newProduct, existingProduct);
    GdnPreconditions.checkArgument(
        GdnMandatoryRequestParameterUtil.getUsername().equals(productReviewer.getApproverAssignee()),
        ErrorCategory.AUTHORIZATION.getMessage() + Constants.INCORRECT_ASSIGNEE_MESSAGE);
    GdnPreconditions.checkArgument(!WorkflowState.PASSED.equals(existingProduct.getState()), PRODUCT_ALREADY_APPROVED);
    newProduct.setRestrictedKeywordsDetected(existingProduct.getRestrictedKeywordsDetected());
    newProduct.setForceReview(existingProduct.isForceReview());
    existingProduct = updateProductDetails(existingProduct, newProduct, Boolean.TRUE);
    responseDto =
      approveProductByVendor(existingProduct, vendorCode, notes, isQuickApproval, productReviewer,
        false);
    return responseDto;
  }

  private List<WorkflowState> getWorkFlowState() {
    List<WorkflowState> states = new ArrayList<>();
      states.add(WorkflowState.IN_REVIEW);
      states.add(WorkflowState.NEED_CORRECTION);
    return states;
  }

  private Date[] getStartAndEndDate(TimeFilterType timeFilterType) {
    Date startDate = new Date();
    Date endDate = new Date();
    Calendar calendar = Calendar.getInstance();
    ZoneId zoneId = TimeZone.getTimeZone(JKT_TIME_ZONE).toZoneId();
    TimeZone.setDefault(TimeZone.getTimeZone(JKT_TIME_ZONE));
    if (!TimeFilterType.ALL.equals(timeFilterType) && !TimeFilterType.FIVE_DAYS_AGO.equals(timeFilterType) && EnumUtils
        .isValidEnum(TimeFilterType.class, timeFilterType.name())) {
      if (TimeFilterType.TODAY.equals(timeFilterType)) {
        endDate = calendar.getTime();
        startDate = Date.from(Instant.from(Instant.now().atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
      } else if (TimeFilterType.YESTERDAY.equals(timeFilterType)) {
        startDate = Date.from(
            Instant.from(Instant.now().minus(Duration.ofDays(1)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
        endDate = Date.from(Instant.from(Instant.now().atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
      } else if (TimeFilterType.TWO_DAYS_AGO.equals(timeFilterType)) {
        startDate = Date.from(
            Instant.from(Instant.now().minus(Duration.ofDays(2)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
        endDate = Date.from(
            Instant.from(Instant.now().minus(Duration.ofDays(1)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
      } else if (TimeFilterType.THREE_TO_FIVE_DAYS_AGO.equals(timeFilterType)) {
        startDate = Date.from(
            Instant.from(Instant.now().minus(Duration.ofDays(5)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
        endDate = Date.from(
            Instant.from(Instant.now().minus(Duration.ofDays(2)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
      }
    } else if (TimeFilterType.FIVE_DAYS_AGO.equals(timeFilterType)) {
      startDate = null;
      endDate = Date.from(
          Instant.from(Instant.now().minus(Duration.ofDays(5)).atZone(zoneId).truncatedTo(ChronoUnit.DAYS)));
    } else {
      startDate = null;
      endDate = null;
    }
    Date[] startAndEndDates = new Date[2];
    startAndEndDates[0] = startDate;
    startAndEndDates[1] = endDate;
    return startAndEndDates;
  }

  private void validateProductInfo(Product product, String productCode) throws NotFoundException {
    String msg = "product not found";
    if (product == null) {
      if (Objects.nonNull(productCode)) {
        product = this.productRepository.findByProductCode(productCode);
        if (Objects.nonNull(product)) {
          msg = "Product is already in " + product.getState().name() + " state.";
        }
      }
      throw new NotFoundException(msg);
    }
  }

  private List<VendorCapacityDTO> vendors(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO) {
    List<VendorCapacityDTO> vendorCapacityDTOs = new ArrayList<>();
    List<Object[]> vendorCapacities = this.vendorRepository.countAllVendorRemainingCapacity();
    for (Object[] vendorCapacity : vendorCapacities) {
      vendorCapacityDTOs.add(this.vendorUtils.convertToVendorCapacityDTO(vendorCapacity));
    }
    return vendorCapacityDTOs;
  }

  @Override
  public Slice<Object[]> findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Date updatedDate, Pageable pageable) {
    return productRepository.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(updatedDate, pageable);
  }

  @Override
  public Slice<Product> findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Date updatedDate,
      Pageable pageable) {
    return productRepository.findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(updatedDate,
        pageable);
  }

  @Override
  @Transactional(timeout = 600, readOnly = false, rollbackFor = Exception.class)
  public void deleteProducts(String storeId, List<String> productIds, List<String> productCodes, int batchSize) {
    Slice<String> items;
    Pageable pageable;
    int page = 0;
    do {
      List<String> itemIds;
      pageable = PageRequest.of(page, batchSize);
      items = productItemRepository.findByProductIds(productIds, pageable);
      itemIds = items.getContent();
      if (CollectionUtils.isNotEmpty(itemIds)) {
        log.info("Deleting products for items : {}", itemIds);
        deleteItemData(itemIds);
      }
      page++;
    } while (items.hasNext());
    deleteProductData(storeId, productIds, productCodes);
    productReviewerService.deleteByProductCodesIn(productCodes);
    productRepository.deleteById(productIds);
  }

  private void deleteProductData(String storeId, List<String> productIds, List<String> productCodes) {
    productImageRepository.deleteByProductIds(productIds);
    productAttributeRepository.deleteByProductIds(productIds);
    productItemRepository.deleteByProductIds(productIds);
    productDistributionTaskRepository.deleteByProductIds(productIds);
    taskHistoryRepository.deletebyProductCodes(storeId, productCodes);
  }

  private void deleteItemData(List<String> productItemIds) {
    productItemImageRepository.deleteByProductItemIds(productItemIds);
    productItemAttributeRepository.deleteByProductItemIds(productItemIds);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product sendProductBackToVendor(String productCode) throws Exception {
    ProductWorkflowStatusResponse workflowStatus = productBusinessPartnerService.getWorkflowStatus(productCode);
    if (Objects.isNull(workflowStatus) || MapUtils.isEmpty(workflowStatus.getStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, PRODUCT_NOT_FOUND_IN_PBP + productCode);
    }
    if (workflowStatus.getStates().contains(Constants.DRAFT) || ((!workflowStatus.isReviewPending()) && workflowStatus
        .getStates().contains(Constants.ACTIVE))) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, PRODUCT_INVALID_STATE + productCode);
    }
    Product product = productRepository.findByProductCode(productCode);
    product.setMarkForDelete(Boolean.FALSE);
    product.setState(WorkflowState.IN_REVIEW);
    product.setQcRetryCount(0);
    productDistributionTaskService.clearPresentDistributionTaskAndCreateNewTask(product);
    return productRepository.save(product);
  }

  @Override
  @Transactional
  public List<Product> republishFinalQcProductsForApproval(int qcRetryCount, int deltaHours, int batchSize) {
    List<Product> productList = productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(qcRetryCount,
            new Date(System.currentTimeMillis() - TimeUnit.HOURS.toMillis(deltaHours)), PageRequest.of(0, batchSize));
    List<Product> updatedProducts = Optional.ofNullable(productList).orElseGet(Collections::emptyList).stream()
        .map(this::publishProductsAndUpdateQcRetryCount)
        .filter(product -> StringUtils.isNotEmpty(product.getProductCode())).collect(Collectors.toList());
    sendEmailForProductsAboveQcRetryCount(qcRetryCount, batchSize);
    return updatedProducts;
  }

  @Override
  @Transactional
  public void retryFinalQCProducts(String productCode) {
    Product product = productRepository.findByProductCode(productCode);
    if (WorkflowState.PASSED.equals(product.getState()) && !product.isMarkForDelete()
        && product.getQcRetryCount() > qcProductsRetryCount) {
      publishProductsAndUpdateQcRetryCount(product);
    }
  }

  private Product publishProductsAndUpdateQcRetryCount(Product product) {
    Product updatedProduct = new Product();
    PrdProductResponse xProductResponse = null;
    try {
      log.info("Publishing approve product event for productCode : {}", product.getProductCode());
      this.productUtils.initializeAllProductDetails(product);
      ProductResponse productResponse =
          this.productServiceRepository.getProductBasicDetailByProductCode(product.getProductCode());
      this.productServiceRepository.clearProductCacheSyncByProductIdAndProductCode(productResponse.getId(),
          product.getProductCode());
      ProductWorkflowStatusResponse productDetailResponseFromPbp =
          this.productWorkflowRepository.getWorkflowStatus(product.getProductCode());
      if (!productDetailResponseFromPbp.isReviewPending()) {
        List<PrdProductResponse> prdProductResponses =
            this.productServiceRepository.getProductBasicDetailByProductCodeFromXProduct(product.getProductCode());
        xProductResponse =
            Optional.ofNullable(prdProductResponses).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
                .findFirst().orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                    " in x-product for productCode" + product.getProductCode()));
      }
      if (Objects.nonNull(xProductResponse) && !xProductResponse.isMarkForDelete()) {
        log.info("removing product {} as it is mfd false in x-product and isReviewPending false in PBP",
            product.getProductCode());
        updatedProduct = deleteProduct(product);
      } else if (Objects.nonNull(xProductResponse) && xProductResponse.isTakenDown()) {
        log.info("removing product from PDT and activating it in x-product as product {} is takenDown in "
                + "x-product and isReviewPending false in PBP",
            product.getProductCode());
        updatedProduct = deleteProduct(product);
        this.productServiceRepository.processProductVendorSearchAutoHeal(GdnMandatoryRequestParameterUtil.getStoreId(),
            product.getProductCode());
      } else if (product.isRevised()) {
        this.approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, false);
      } else if (product.isEdited()) {
        this.approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
      } else {
        this.approvedProductPublisherService.publishVendorApprovedEvent(product, false);
      }
      productRepository.updateQcRetryCount(product.getProductCode());
    } catch (Exception e) {
      productRepository.updateQcRetryCount(product.getProductCode());
      log.error("error while sending product details to kafka for final approval. productCode: {}",
          product.getProductCode(), e);
    }
    return updatedProduct;
  }

  private Product deleteProduct(Product product) {
    product.setMarkForDelete(Boolean.TRUE);
    return this.productRepository.save(product);
  }

  private void sendEmailForProductsAboveQcRetryCount(int qcRetryCount, int batchSize) {
    List<StuckProductsDTO> productsAboveQcRetryCount = productRepository.getProductsAboveQcRetryCount(qcRetryCount);
    if (CollectionUtils.isNotEmpty(productsAboveQcRetryCount)) {
      log.info("Sending email for products above retry count, qcRetryCount : {}, batchSize : {}", qcRetryCount,
          batchSize);
      MessageEmailRequest email = ConverterUtil.getStuckProductMessageEmailRequest(batchSize, productsAboveQcRetryCount,
          sendStuckProductSummaryEmailAddress, sendStuckProductSummaryEmailAddressCc);
      try {
        kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
      } catch (Exception e) {
        log.error("Product Retry Summary Email failed to send, {}", email, e);
      }
    }
  }

  @Transactional
  @Override
  public List<Product> getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
      String storeId, List<WorkflowState> states, boolean markForDelete, boolean postLive, Date date, int pageSize) {
    GdnPreconditions.checkArgument(Objects.nonNull(storeId), STORE_ID_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(states), STATES_MUST_NOT_BE_EMPTY);
    List<String> stateInString = states.stream().map(state -> state.toString()).collect(Collectors.toList());
    return productRepository.findUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        storeId, stateInString, markForDelete, postLive, date, pageSize);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProductAsPostLiveTrue(String productCode) throws Exception {
    productRepository.updateProductPostLiveFlag(productCode, true);
    Product savedProduct = getAllProductDetailsByCode(productCode);
    approvedProductPublisherService.publishVendorApprovedEvent(savedProduct, true);
  }


  @Override
  public void deleteOriginalImagesForProductAndItems(Product product) {
    if (deleteOriginalImages) {
      Set<String> locationPaths = this.fetchLocationPathsForDeletion(product);
      for (String locationPath : locationPaths) {
        try {
          if (!locationPath.startsWith(File.separator)) {
            locationPath = File.separator + locationPath;
          }
          log.info("Deleting image from the location : {}", locationPath);
          if (deleteImagesEventBasedEnabled) {
            kafkaProducer.send(kafkaTopicPropertiesConsumer.getDeleteOriginalImageEvent(),
                product.getProductCode(),
                DeleteOriginalImageEventModel.builder().locationPath(locationPath).build());
          } else {
            fileStorageService.deleteOriginalImages(locationPath);
          }
        } catch (Exception e) {
          log.error("Error while deleting image from location {}", locationPath, e);
        }
      }
    }
  }

  private Set<String> fetchLocationPathsForDeletion(Product product) {
    Set<String> locationPaths = new HashSet<>();
    Set<String> nonOriginalLocationPaths = new HashSet<>();
    List<ProductImage> productImages = productImageRepository.findByProductId(product.getId());
    List<ProductItemImage> productItemImages = productItemImageRepository.findByProductId(product.getId());
    locationPaths.addAll(Optional.ofNullable(productImages).orElse(new ArrayList<>()).stream()
        .filter(productImage -> Boolean.TRUE.equals(productImage.getOriginalImage())).map(ProductImage::getLocationPath)
        .collect(Collectors.toSet()));
    locationPaths.addAll(Optional.ofNullable(productItemImages).orElse(new ArrayList<>()).stream()
        .filter(productItemImage -> Boolean.TRUE.equals(productItemImage.getOriginalImage()))
        .map(ProductItemImage::getLocationPath).collect(Collectors.toSet()));
    nonOriginalLocationPaths.addAll(Optional.ofNullable(productImages).orElse(new ArrayList<>()).stream()
        .filter(productImage -> Boolean.FALSE.equals(productImage.getOriginalImage())).map(ProductImage::getLocationPath)
        .collect(Collectors.toSet()));
    nonOriginalLocationPaths.addAll(Optional.ofNullable(productItemImages).orElse(new ArrayList<>()).stream()
        .filter(productItemImage -> Boolean.FALSE.equals(productItemImage.getOriginalImage()))
        .map(ProductItemImage::getLocationPath).collect(Collectors.toSet()));
    locationPaths.removeAll(nonOriginalLocationPaths);
    return locationPaths;
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public PublishAndSavedProductAndHistoryModel updateImageQcResponseByProductCode(
      ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent, Product product) throws Exception {
    boolean doPublishEvent = false;
    boolean publishNeedRevisionEvent = false;
    Pair<Boolean, InternalHistoryEventModel> doPublishAndHistoryEventModel = Pair.of(false, null);
    if (Objects.nonNull(product)) {
      product.setProductPredictionScore(imageQcProcessedResponseDomainEvent.getProductPredictionScore());
      product.setImageViolations(StringUtils.isNotEmpty(imageQcProcessedResponseDomainEvent.getImageViolations()) ?
          imageQcProcessedResponseDomainEvent.getImageViolations() :
          null);
      product.setTextViolations(StringUtils.isNotEmpty(imageQcProcessedResponseDomainEvent.getTextViolations()) ?
          imageQcProcessedResponseDomainEvent.getTextViolations() :
          null);
      product.setPredictedBrand(imageQcProcessedResponseDomainEvent.getPredictedBrand());
      if (Objects.nonNull(imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse())
          && imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse().isAutoNeedRevision()) {
        product.setAutoNeedRevision(true);
        publishNeedRevisionEvent = true;
      }
      product.setForceReview(imageQcProcessedResponseDomainEvent.isForceReview());
      ProductImageQcFeedbackRequest productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
      productImageQcFeedbackRequest.setProductCode(imageQcProcessedResponseDomainEvent.getProductCode());
      productImageQcFeedbackRequest.setSystemFeedback(imageQcProcessedResponseDomainEvent.getImageQcResponse());
      productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, false, true);
      doPublishAndHistoryEventModel = autoApproveProduct(imageQcProcessedResponseDomainEvent, product, doPublishEvent);
    }
    log.info("Saving updated product. product = {} ", product);
    Product savedProduct = productRepository.saveAndFlush(product);
    publishAutoNeedRevisionEvent(imageQcProcessedResponseDomainEvent, product, publishNeedRevisionEvent);
    return PublishAndSavedProductAndHistoryModel.builder()
        .internalHistoryEventModel(doPublishAndHistoryEventModel.getRight())
        .doPublish(doPublishAndHistoryEventModel.getLeft()).savedProduct(savedProduct).build();
  }

  private Pair<Boolean, InternalHistoryEventModel> autoApproveProduct(
      ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent,
      Product product, boolean doPublishEvent) throws Exception {
    InternalHistoryEventModel internalHistoryEventModel = null;
    if (AutoApprovalType.CONTENT_AND_IMAGE.equals(imageQcProcessedResponseDomainEvent.getAutoApprovalType())
        && !product.isAutoNeedRevision()) {
      ProductReviewer productReviewer =
          productReviewerService.findProductReviewerByProductCode(product.getProductCode());
      String notes = validateImageQcProcessedResponseDomainEvent(product, productReviewer);
      log.info("Notes after product validation of auto approval. notes = {} ", notes);
      if (StringUtils.isEmpty(notes)) {
        String state = productServiceRepository.getProductStatus(product.getStoreId(),
            imageQcProcessedResponseDomainEvent.getProductCode());
        if (StringUtils.isNotEmpty(state) && Constants.ACTIVE.equalsIgnoreCase(state)) {
          notes = updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
          log.info("Updated state and notes of product after auto approval. state = {}, notes = {} ",
              product.getState().getDesc(), notes);
          //TODO IMAGE DELETION
          productUtils.removeOriginalImagesFromProduct(product);
          doPublishEvent = true;
        } else {
          log.info("Auto Approval failed because product is not active for productCode = {} ", product.getProductCode());
          notes = Constants.NOT_ACTIVE_ERROR_MESSAGE;
          if (Objects.nonNull(productReviewer)) {
            productReviewer.setAssignedDate(new Date());
            productReviewer.setApproverAssignee(Constants.AUTO_APPROVED);
          } else {
            log.error("ProductReviewer is null productReviewer = {} ", productReviewer);
          }
          productAutoApprovalService.addProductsToAutoApprovalTable(product.getStoreId(),
              Arrays.asList(imageQcProcessedResponseDomainEvent.getProductCode()), Collections.emptyMap());
        }
        if (Objects.nonNull(productReviewer)) {
          productReviewerService.saveAndFlush(productReviewer);
        } else {
          log.error("ProductReviewer is null productReviewer = {} ", productReviewer);
        }
      }
      log.info("Submitting product auto approval history. productCode = {}, notes = {}, state = {} ",
          product.getProductCode(), notes, product.getState().getDesc());
      if (doPublishEvent) {
        internalHistoryEventModel =
            ConverterUtil.convertToInternalHistoryModel(GdnMandatoryRequestParameterUtil.getStoreId(),
                product.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.AUTO_APPROVED,
                notes);
      } else {
        internalHistoryEventModel =
            ConverterUtil.convertToInternalHistoryModel(GdnMandatoryRequestParameterUtil.getStoreId(),
                product.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(),
                Constants.CANNOT_AUTO_APPROVED, notes);
      }
    }
    return Pair.of(doPublishEvent, internalHistoryEventModel);
  }

  private void publishAutoNeedRevisionEvent(ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent,
      Product product, boolean publishNeedRevisionEvent) {
    if(publishNeedRevisionEvent) {
      AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent =
          new AutoNeedRevisionDomainEvent(product.getStoreId(), product.getProductCode(), new ArrayList<>(
              imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse().getPredictionTypeSet()));
      autoNeedRevisionDomainEvent.setNotes(
          imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse().getNotes());
      autoNeedRevisionDomainEvent.setContentNeedRevision(
          imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse().isContentNeedRevision());
      kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT,
          product.getProductCode(), autoNeedRevisionDomainEvent);
      log.info("Publish Auto need revision event com.gdn.pbp.product.auto.need.revision message : {} ",
          autoNeedRevisionDomainEvent);
    }
  }

  public String updateStatesForAutoApproveCapableProducts(Product product, String notes,
      ProductReviewer productReviewer) throws Exception {
    Date updatedDate = new Date();
    ProductDistributionTask productDistributionTask =
        this.productDistributionTaskService.findByProductId(product.getId());
    if (product.isEdited()) {
      if (WorkflowState.IN_REVIEW.equals(product.getState())) {
        product.setState(WorkflowState.PASSED);
        notes = getNotesByReviewType(product.getReviewType());
        productDistributionTaskService.updateState(productDistributionTask, WorkflowState.PASSED);
      }
    } else {
      product.setState(WorkflowState.PASSED);
      notes = Constants.CONTENT_AND_IMAGE_AUTO_APPROVAL;
      ConverterUtil.setReviewAutoApprovalDetails(updatedDate, productReviewer);
      productDistributionTaskService.updateState(productDistributionTask, WorkflowState.PASSED);
    }
    return notes;
  }

  private String getNotesByReviewType(ReviewType reviewType) {
    String notes = StringUtils.EMPTY;
    if (Objects.isNull(reviewType) || ReviewType.CONTENT_AND_IMAGE.equals(reviewType)) {
      notes = Constants.CONTENT_AND_IMAGE_AUTO_APPROVAL;
    } else if (ReviewType.CONTENT.equals(reviewType)) {
      notes = Constants.CONTENT_AUTO_APPROVAL;
    } else {
      notes = Constants.IMAGE_AUTO_APPROVAL;
    }
    return notes;
  }

  private String validateImageQcProcessedResponseDomainEvent(Product product, ProductReviewer productReviewer) {
    if (!product.isPostLive()) {
      return Constants.PRODUCT_PRELIVE_ERROR_MESSAGE;
    }
    if (product.isPostLive() && !product.isRevised() && !Constants.BRAND_APPROVAL_STATUS
        .equals(product.getBrandApprovalStatus())) {
      return Constants.BRAND_APPROVAL_ERROR_MESSAGE;
    }
    if (!product.isRevised() && Objects.nonNull(productReviewer) && Objects
        .nonNull(productReviewer.getApproverAssignee())) {
      return Constants.ALREADY_ASSIGNED_ERROR_MESSAGE;
    }
    if (!product.isRevised() && !WorkflowState.IN_REVIEW.equals(product.getState())) {
      return Constants.PRODUCT_STATE_ERROR_MESSAGE;
    }
    return StringUtils.EMPTY;
  }

  @Override
  public void publishAutoApprovalEvents(boolean doPublishEvent, Product product) {
    log.info("Publish auto approval events to PBP. doPublishEvent = {}, isEdited = {} ", doPublishEvent,
        product.isEdited());
    if (doPublishEvent) {
      if (product.isEdited()) {
        approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
      } else if (product.isRevised()) {
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, false);
      } else {
        approvedProductPublisherService
            .publishAutoApprovalEvent(ConverterUtil.toPDTAutoApprovalEventModel(product.getProductCode()));
      }
    }
  }

  @Override
  @Transactional(readOnly = false)
  public List<String> updateBrandApprovalStatus(
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel) {
    List<Object[]> productIdAndProductItemIdAndProductCodeList =
        productRepository.findIdByBrandCodeAndMarkForDeleteFalse(
            brandApprovedOrRejectedDomainEventModel.getBrandRequestCode());

    List<String> productIds = new ArrayList<>();
    List<String> productItemIds = new ArrayList<>();
    List<String> productCodes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productIdAndProductItemIdAndProductCodeList)) {
      for (Object[] objects : productIdAndProductItemIdAndProductCodeList) {
        productIds.add(String.valueOf(objects[0]));
        productItemIds.add(String.valueOf(objects[1]));
        productCodes.add(String.valueOf(objects[2]));
      }
    }

      if (CollectionUtils.isNotEmpty(productIds)) {
        productRepository.updateBrandDetail(Objects.nonNull(brandApprovedOrRejectedDomainEventModel.getBrandCode()) ?
                brandApprovedOrRejectedDomainEventModel.getBrandCode() :
                brandApprovedOrRejectedDomainEventModel.getBrandRequestCode(),
            brandApprovedOrRejectedDomainEventModel.getBrandName(),
            brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(), productIds);
        productAttributeRepository.updateValueByNameAndProductIds(Constants.BRAND,
            brandApprovedOrRejectedDomainEventModel.getBrandName(), productIds);
      }

      if (CollectionUtils.isNotEmpty(productItemIds)) {
        productItemAttributeRepository.updateValueByNameAndProductIds(Constants.BRAND,
            brandApprovedOrRejectedDomainEventModel.getBrandName(), productItemIds);
      }

    return productCodes;
  }

  @Override
  public Page<Product> findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      Pageable pageable) {
    return productRepository.findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId,
        pageable);
  }

  @Override
  public Page<Product> findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(String storeId, Date startDate,
      Date endDate, Pageable pageable) {
    return productRepository.findByStoreIdAndUpdatedDateBetween(storeId, startDate, endDate, pageable);
  }

  @Override
  public Map<String, Object> getReviewConfigProductCountByVendorAndConfig(String storeId, String vendorCode,
      boolean postLive) throws Exception {
    try {
      return solrVendorCollectionService.getReviewConfigCountsByVendorAndConfig(storeId, vendorCode, postLive);
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Error fetching counts from solr, fallback to db query, ", e);
      throw e;
    }
  }

  @Override
  public void updateApprovedProductData(Product existingProduct, Product newProduct) {
    productUtils.replaceProductDetails(existingProduct, newProduct, true);
    existingProduct = this.productUtils.replaceProductImageAndProductItemImages(existingProduct, newProduct);
    this.productUtils.regenerateProductImageDetails(existingProduct);
  }

  @Override
  public Product updateRevisedProductData(Product existingProduct, Product newProduct, List<String> modifiedFields)
      throws IOException {
    if (ReviewType.IMAGE.equals(newProduct.getReviewType()) && modifiedFields.contains(CONTENT)) {
      newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    } else if (ReviewType.CONTENT.equals(newProduct.getReviewType()) && modifiedFields.contains(IMAGE)) {
      newProduct.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    }
    productUtils.replaceProductDetails(existingProduct, newProduct, false);
    existingProduct = this.productUtils.replaceProductImageAndProductItemImages(existingProduct, newProduct);
    existingProduct.getProductItems().stream().forEach(productItem -> productItem.setItemNotes(null));
    existingProduct.setRestrictedKeywordsDetected(newProduct.getRestrictedKeywordsDetected());
    this.productUtils.regenerateProductImageDetails(existingProduct);
    updateProductNotesForRevisedProducts(existingProduct, modifiedFields);
    existingProduct.setRevised(true);
    existingProduct.setReviewType(newProduct.getReviewType());
    existingProduct.setPostLive(newProduct.isPostLive());
    existingProduct.setState(newProduct.getState());
    existingProduct.setAutoNeedRevision(false);
    existingProduct.setForceReview(newProduct.isForceReview());
    existingProduct.setAppealedProduct(newProduct.isAppealedProduct());
    existingProduct.setImageViolations(newProduct.getImageViolations());
    existingProduct.setTextViolations(newProduct.getTextViolations());
    existingProduct.setDistributionMappingStatus(newProduct.getDistributionMappingStatus());
    ImageUtils.setActiveFlagInProductAndItemImages(existingProduct);
    existingProduct = this.productRepository.save(existingProduct);
    return existingProduct;
  }

  @Override
  public void updateProductNotesForRevisedProducts(Product product, List<String> modifiedFields)
      throws IOException {
    ProductNotesResponse productNotesResponse = new ProductNotesResponse();
    if (Objects.nonNull(product.getProductNotes())) {
      productNotesResponse = mapper.readValue(product.getProductNotes(), ProductNotesResponse.class);
    }
    productNotesResponse.setModifiedFields(modifiedFields);
    productNotesResponse.setLastModified(Constants.REVISED);
    product.setProductNotes(mapper.writeValueAsString(productNotesResponse));
  }

  @Override
  public void republishEditedProduct(String productCode) {
    List<WorkflowState> allowedStates = new ArrayList<>();
    allowedStates.add(WorkflowState.PASSED);
    Product product = productRepository.findByProductCodeAndMarkForDeleteFalse(productCode);
    if (Objects.nonNull(product) && (product.isEdited() || product.isRevised()) && allowedStates
        .contains(product.getState())) {
      if (product.isRevised()) {
        approvedProductPublisherService.publishRevisedVendorApprovedEvent(product, false);
      } else {
        approvedProductPublisherService.publishEditedVendorApprovedEvent(product);
      }
    }
  }

  private boolean isProductEligibleForAutoApproval(Product product, int maxNumberOfDaysToApproveAssigneeProducts,
      SimpleStringDTO notEligibleReason) {
    ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(product.getProductCode());
    if (product.isPostLive()) {
      if (Constants.APPROVED.equalsIgnoreCase(product.getBrandApprovalStatus())) {
        if (WorkflowState.IN_REVIEW.equals(product.getState())) {
          if (product.isRevised()) {
            return true;
          } else {
            boolean productsAssignmentEligibleForAutoApproval =
                Objects.nonNull(productReviewer) && (Objects.isNull(productReviewer.getApproverAssignee())
                    || Constants.AUTO_APPROVED.equals(productReviewer.getApproverAssignee())
                    || isEligibleForAutoApprovalIfAssigned(productReviewer.getAssignedDate(),
                    maxNumberOfDaysToApproveAssigneeProducts));
            if (!productsAssignmentEligibleForAutoApproval) {
              notEligibleReason.setSimpleString(ErrorMessages.AUTO_APPROVAL_PRODUCT_ASSIGNMENT);
            }
            return productsAssignmentEligibleForAutoApproval;
          }
        } else {
          notEligibleReason.setSimpleString(ErrorMessages.AUTO_APPROVAL_PRODUCT_NOT_IN_REVIEW + product.getState());
        }
      } else {
        notEligibleReason.setSimpleString(ErrorMessages.AUTO_APPROVAL_PRODUCT_BRAND_NOT_ACTIVE);
      }
    } else {
      notEligibleReason.setSimpleString(ErrorMessages.AUTO_APPROVAL_PRODUCT_NOT_POST_LIVE);
    }
    return false;
  }

  private boolean isEligibleForAutoApprovalIfAssigned(Date assignedDate, int maxNumberOfDaysToApproveAssigneeProducts) {
    if(Objects.nonNull(assignedDate)) {
      Date currentDate = new Date();
      long diffInMillies = Math.abs(currentDate.getTime() - assignedDate.getTime());
      long assignedBefore = TimeUnit.DAYS.convert(diffInMillies, TimeUnit.MILLISECONDS);
      if ((int) assignedBefore >= maxNumberOfDaysToApproveAssigneeProducts) {
        return true;
      }
    }
    return false;
  }

  @Override
  public void autoApprovePendingProducts(String storeId, List<ProductAutoApproval> productAutoApprovalList,
      int maxNumberOfDaysToApproveAssigneeProducts) {
    Map<String, Product> productMap = Optional.ofNullable(productRepository.findByProductCodeInAndMarkForDeleteFalse(
            productAutoApprovalList.stream().map(ProductAutoApproval::getProductCode).collect(Collectors.toList())))
        .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(Product::getProductCode, Function.identity()));
    for (ProductAutoApproval productForAutoApproval : productAutoApprovalList) {
      try {
        Product product = productMap.getOrDefault(productForAutoApproval.getProductCode(), null);
        SimpleStringDTO notEligibleReason = new SimpleStringDTO();
        if (Objects.nonNull(product) && isProductEligibleForAutoApproval(product,
            maxNumberOfDaysToApproveAssigneeProducts, notEligibleReason)) {
          ReviewType autoApprovalType = product.getReviewType();
          if (Objects.isNull(autoApprovalType)) {
            autoApprovalType = ReviewType.CONTENT_AND_IMAGE;
          }
          log.info("Publishing event for auto approval criteria check {}, for productCode : {} ",
              DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME, product.getProductCode());
          publishEventAndUpdateStatus(productForAutoApproval,
              new ProductAutoApprovalEventModel(product.getStoreId(), product.getProductCode(),
                  product.getCategoryCode(), product.isEdited(), autoApprovalType.toString(), product.isRevised(),
                  productForAutoApproval.getCategoryCode()));
        } else {
          if (Objects.nonNull(product) && StringUtils.isNotEmpty(productForAutoApproval.getCategoryCode())) {
            log.info("Publish Auto approval event for only category change for product {} and categoryCode {} ",
                product.getProductCode(), productForAutoApproval.getCategoryCode());
            publishEventAndUpdateStatus(productForAutoApproval,
                new ProductAutoApprovalEventModel(product.getStoreId(), product.getProductCode(),
                    product.getCategoryCode(), product.isEdited(), ReviewType.CONTENT_AND_IMAGE.toString(),
                    product.isRevised(), productForAutoApproval.getCategoryCode(), true));
          } else {
            productForAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.NA);
            productForAutoApproval.setReasonOfFailure(notEligibleReason.getSimpleString());
            productAutoApprovalService.updateProductAutoApprovalDetails(productForAutoApproval);
          }
        }
      } catch (Exception e) {
        log.error("Error in system while auto approving product : {} ", productForAutoApproval.getProductCode(), e);
        productForAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.FAILED);
        productForAutoApproval.setCategoryCode(StringUtils.EMPTY);
        productAutoApprovalService.updateProductAutoApprovalDetails(productForAutoApproval);
      }
    }
  }

  private void publishEventAndUpdateStatus(ProductAutoApproval productForAutoApproval,
      ProductAutoApprovalEventModel product) {
    kafkaProducer.send(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME, productForAutoApproval.getProductCode(),
        product);
    productForAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.PUBLISHED);
    productAutoApprovalService.updateProductAutoApprovalDetails(productForAutoApproval);
  }

  private ProductReviewer setAssigneesForAutoApprovedProducts(Product product){
    ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(product.getProductCode());
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApproverAssignee(Constants.AUTO_APPROVED);
    productReviewer.setApprovedDate(new Date());
    return productReviewer;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Pair<AutoApprovalStatus, InternalHistoryEventModel> autoApproveOfPendingProductsAfterEligibilityCheck(Product product
      , boolean isEligibleForAutoApproval, int maxNumberOfDaysToApproveAssignedProducts) throws Exception {
    AutoApprovalStatus autoApprovalStatus = AutoApprovalStatus.NA;
    InternalHistoryEventModel internalHistoryEventModel = null;
    if (isEligibleForAutoApproval) {
      SimpleStringDTO notEligibleReason = new SimpleStringDTO();
      if (isProductEligibleForAutoApproval(product, maxNumberOfDaysToApproveAssignedProducts, notEligibleReason)) {
        product.setState(WorkflowState.PASSED);
        ProductReviewer productReviewer = setAssigneesForAutoApprovedProducts(product);
        //TODO IMAGE DELETION
        productUtils.removeOriginalImagesFromProduct(product);
        String notes = Constants.CONTENT_AND_IMAGE_AUTO_APPROVAL;
        if(ReviewType.CONTENT.equals(product.getReviewType())){
          notes = Constants.CONTENT_AUTO_APPROVAL;
        } else if(ReviewType.IMAGE.equals(product.getReviewType())){
          notes = Constants.IMAGE_AUTO_APPROVAL;
        }
        log.info("Submitting product auto approval history. productCode = {}, notes = {}, state = {} ",
            product.getProductCode(), notes, product.getState().getDesc());
        internalHistoryEventModel =
            ConverterUtil.convertToInternalHistoryModel(GdnMandatoryRequestParameterUtil.getStoreId(),
                product.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(), Constants.AUTO_APPROVED,
                notes);
        log.info("Saving updated product. product = {} ", product);
        productRepository.saveAndFlush(product);
        productReviewerService.save(productReviewer);
        autoApprovalStatus = AutoApprovalStatus.SUCCESS;
      } else {
        log.info("Product assign to an approver for productCode : {} not eligible reason : {} ",
            product.getProductCode(), notEligibleReason.getSimpleString());
        productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(product.getStoreId(),
            product.getProductCode(), autoApprovalStatus, true);
      }
    } else {
      log.info("Product not eligible for auto approval : {} ", product.getProductCode());
      productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(product.getStoreId(),
          product.getProductCode(), autoApprovalStatus, true);
    }
    return Pair.of(autoApprovalStatus, internalHistoryEventModel);
  }

  private void saveTaskHistory(Product product, String reason) {
    TaskHistory taskHistory =
        new TaskHistory(product.getProductCode(), product.getProductName(), product.getCategoryCode(),
            product.getCategoryName(), product.getCurrentVendor(), reason, product.getState(), product.getStoreId(),
            product.getUpdatedBy(), productDistributionTaskRepository.getTaskCodeForProduct(product.getId()));
    this.taskHistoryRepository.save(taskHistory);
  }

  private boolean isProductUnAssigned(Product product) {
    ProductReviewer productReviewer =
        productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
            product.getProductCode());
    return WorkflowState.IN_REVIEW.equals(product.getState()) && Objects.nonNull(productReviewer)
        && Objects.isNull(productReviewer.getApproverAssignee());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Product updateProductToAutoNeedRevision(String storeId,
      AutoNeedRevisionRequest autoNeedRevisionRequest, boolean validateAssignment) {
    Product product = productRepository.findByProductCodeAndMarkForDeleteFalse(autoNeedRevisionRequest.getProductCode());
    if (Objects.isNull(product)) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "The product is not present");
    } else if ((!validateAssignment && !isProductUnAssigned(product)) || isProductUnAssigned(product)) {
      product.setState(WorkflowState.valueOf(autoNeedRevisionRequest.getState()));
      product.setMarkForDelete(true);
      product.setAutoNeedRevision(true);
      product.setAppealedProduct(false);
      product = productRepository.saveAndFlush(product);
      return product;
    }
    else {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, "The product is already assigned for approval");
      }
  }

  @Override
  @Transactional(readOnly = false)
  public void updateProductTaskAndHistory(Product product, AutoNeedRevisionRequest autoNeedRevisionRequest) {
    updateProductDistributionTask(product);
    saveTaskHistory(product, autoNeedRevisionRequest.getReason());
  }

  @Override
  public void autoNeedReviseForPendingProducts(String storeId, List<ProductActionRetry> productActionRetryList) {
    Map<String, Product> productMap =
        Optional.ofNullable(productRepository.findByProductCodeInAndMarkForDeleteFalse(
            productActionRetryList.stream().map(ProductActionRetry::getProductCode).collect(Collectors.toList())))
            .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(Product::getProductCode, Function.identity()));
    for (ProductActionRetry productActionRetry : productActionRetryList) {
      try {
        log.info("Retrying auto need revision for product : {} ", productActionRetry.getProductCode());
        Product product = productMap.getOrDefault(productActionRetry.getProductCode(), null);
        if (Objects.nonNull(product) && isProductUnAssigned(product)) {
          RetryNeedRevisionRequest retryNeedRevisionRequest =
              new RetryNeedRevisionRequest(storeId, product.getProductCode(), productActionRetry.getData());
          RetryAutoNeedRevisionResponse autoNeedRevisionResponse =
              productServiceRepository.retryAutoNeedRevision(storeId, retryNeedRevisionRequest);
          if (!autoNeedRevisionResponse.isProductActive()) {
            productActionRetryService.updateProductActionRetryDetails(productActionRetry);
          } else {
            productActionRetry.setStatus(ActionRetryStatus.SUCCESS);
            productActionRetryService.updateProductActionRetryDetails(productActionRetry);
          }
        } else {
          productActionRetry.setStatus(ActionRetryStatus.NA);
          productActionRetryService.updateProductActionRetryDetails(productActionRetry);
        }
      } catch (Exception e) {
        log.error("Error while retrying auto need revision of pending products : {} ", productActionRetry.getProductCode(), e);
        productActionRetry = productActionRetryService.getProductActionRetryByProductCodeAndAction(
            storeId, productActionRetry.getProductCode(), productActionRetry.getAction());
        if(Objects.nonNull(productActionRetry)) {
          productActionRetry.setStatus(ActionRetryStatus.FAILED);
          productActionRetryService.updateProductActionRetryDetails(productActionRetry);
        }
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public QuickApprovalResponse quickApproveProduct(String vendorCode, String productCode, String notes,
      ProductReviewer productReviewer, boolean isBulkAction, Product existingProduct) throws Exception {
    if(existingProduct.isMarkForDelete()){
      VendorQuickApprovalResponse vendorQuickApprovalResponse = new VendorQuickApprovalResponse();
      vendorQuickApprovalResponse.setErrorCodes(Collections.singletonList(ApiErrorCode.PRODUCT_IS_ALREADY_APPROVED.getCode()));
      log.error("Product is not eligible for approval, already in passed state : {} ", productCode);
      return new QuickApprovalResponse(vendorQuickApprovalResponse, existingProduct);
    }
    CategoryDetailResponse categoryDetailResponse =
        productServiceRepository.getCategoryDetailByCategoryCode(existingProduct.getCategoryCode());
    VendorQuickApprovalResponse vendorQuickApprovalResponse =
        ValidationUtil.vendorQuickApprovalProductValidation(existingProduct,
          categoryDetailResponse, isBulkAction, validateAndHealQuickApprovalEnabled);
    existingProduct = autoHealProductDataForApprovalIfEligible(productCode, existingProduct,
      vendorQuickApprovalResponse);
    String brandCode = StringUtils.isNotBlank(existingProduct.getBrandCode()) ?
        existingProduct.getBrandCode() :
        productUtils.getBrandCodeByBrandName(existingProduct.getBrand());
    if (!productUtils.validateProtectedBrand(brandCode, existingProduct.getBusinessPartnerCode())) {
      log.warn("Product code : {}, error {}", productCode,
          ApiErrorCode.SELLER_NOT_AUTHORISED_TO_CREATE_PRODUCT.getDesc());
      vendorQuickApprovalResponse.getErrorCodes().add(ApiErrorCode.SELLER_NOT_AUTHORISED_TO_CREATE_PRODUCT.getCode());
    }
    if (Objects.isNull(existingProduct.getCurrentVendor())) {
      log.warn("Product code {} is not yet assigned to the vendor ",
        existingProduct.getProductCode());
      vendorQuickApprovalResponse.getErrorCodes().add(ApiErrorCode.PRODUCT_NOT_ASSIGNED.getCode());
    }
    if (Optional.of(existingProduct.getProductItems()).orElse(Collections.emptyList()).stream()
      .noneMatch(Predicate.not(ProductItem::isMarkForDelete))) {
      log.warn("Product code {} has no eligible Items in PDT for Approval ",
        existingProduct.getProductCode());
      vendorQuickApprovalResponse.getErrorCodes().add(ApiErrorCode.PRODUCT_ITEMS_ARE_MFD_TRUE.getCode());
    }
    recheckHealedProductData(existingProduct, vendorQuickApprovalResponse);
    ApproveProductResponseDto responseDto = new ApproveProductResponseDto();
    if (CollectionUtils.isEmpty(vendorQuickApprovalResponse.getErrorCodes())) {
      //approve product
      updateProductDetailsForQuickApproval(existingProduct);
      responseDto =
        approveProductByVendor(existingProduct, vendorCode, notes, true, productReviewer,
          isBulkAction);
    }
    return QuickApprovalResponse.builder().vendorQuickApprovalResponse(vendorQuickApprovalResponse)
        .product(existingProduct).approveProductResponseDto(responseDto).build();
  }

  private void recheckHealedProductData(Product existingProduct,
    VendorQuickApprovalResponse vendorQuickApprovalResponse) {
    if (isEligibleForHealingAttributes(existingProduct)) {
      // checking again if Attributes healed from PCB were MFD true , failing quick approval
      log.warn("Product code {} has no eligible Attributes in PDT for Approval ",
        existingProduct.getProductCode());
      vendorQuickApprovalResponse.getErrorCodes()
        .add(ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode());
    }
    if (isEligibleForHealingProductImages(existingProduct)) {
      // checking again if Product Images healed from PCB were MFD true  , failing quick approval
      log.warn("Product code {} has no eligible Product Images in PDT for Approval ",
        existingProduct.getProductCode());
      vendorQuickApprovalResponse.getErrorCodes()
        .add(ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getCode());
    }
    if (productContainsEmptyItemImages(existingProduct)) {
      log.warn("Product code {} has no eligible Item Images in PDT for Approval ",
        existingProduct.getProductCode());
      vendorQuickApprovalResponse.getErrorCodes()
        .add(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode());
    }
  }

  @Override
  public Product autoHealProductDataForApprovalIfEligible(String productCode,
    Product existingProduct, VendorQuickApprovalResponse vendorQuickApprovalResponse)
    throws Exception {
    Product healProductData = null;
    boolean isAutoHealed = false;
    boolean isProductDataUpdated = false;
    if (validateAndHealQuickApprovalEnabled) {
      if (autoHealAutoApprovalProductData && eligibilityForAutoHeal(vendorQuickApprovalResponse)) {
        healProductData = autoHealProductData(existingProduct, Constants.AUTOHEAL_QUICK_APPROVAL);
        log.info("Product data was auto healed before performing quick apporval for product : {} ",
          productCode);
        List<String> errorCodesForAutoHeal = new ArrayList<>(getErrorCodesForAutoHeal());
        List<String> filteredErrorCodes = new ArrayList<>(vendorQuickApprovalResponse.getErrorCodes());
        filteredErrorCodes.removeIf(errorCodesForAutoHeal::contains);
        vendorQuickApprovalResponse.setErrorCodes(filteredErrorCodes);
        existingProduct = healProductData;
        isAutoHealed = true;
      }
      if (orphanedImageQuickApprovalAutoHealFlag && !isAutoHealed) {
        isProductDataUpdated = validateAndProcessOrphanedImages(existingProduct);
      }
      if (maxImageCountForGcsFileCheck > 0) {
        validateInActiveImagesExistInGcs(existingProduct, vendorQuickApprovalResponse);
      }
      if (StringUtils.isNotBlank(existingProduct.getBrand())) {
        validateAndSetBrandCode(existingProduct, vendorQuickApprovalResponse);
      }
      if (isProductDataUpdated) {
        healProductData = productRepository.save(existingProduct);
        existingProduct = healProductData;
      }
    }

    return existingProduct;
  }

  private void validateInActiveImagesExistInGcs(Product existingProduct,
    VendorQuickApprovalResponse vendorQuickApprovalResponse) {
    Set<String> existingInActiveImages = getExistingInActiveNonOriginalImages(existingProduct);
    Set<String> activeImageLocationsForProductAndProductItem = getActiveImageLocationsForProductItem(existingProduct);
    int totalImageCount = existingInActiveImages.size() + activeImageLocationsForProductAndProductItem.size();
    if (totalImageCount < maxImageCountForGcsFileCheck) {
      List<String> invalidImageLocations = existingInActiveImages.stream().filter(Objects::nonNull)
        .filter(location -> fileStorageService.isSourceImageFileExist(location)).peek(
          location -> log.warn(
            "Product {} has Images that are missing in Gcs or only final images were found ",
            existingProduct.getProductCode())).collect(Collectors.toList());
      List<String> invalidActiveImageLocations =
        activeImageLocationsForProductAndProductItem.stream().filter(Objects::nonNull)
        .filter(location -> fileStorageService.isFinalImageFileExist(location)).peek(
          location -> log.warn(
            "Product {} has Images that are missing in Gcs or only final images were found ",
            existingProduct.getProductCode())).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(invalidImageLocations) || CollectionUtils.isNotEmpty(
        invalidActiveImageLocations)) {
        List<String> errorCodes = new ArrayList<>(vendorQuickApprovalResponse.getErrorCodes());
        errorCodes.add(ApiErrorCode.PRODUCT_IMAGES_NOT_FOUND.getCode());
        vendorQuickApprovalResponse.setErrorCodes(errorCodes);
      }

    }
  }

  private static Set<String> getActiveImageLocationsForProductItem(Product existingProduct) {
    Set<String> activeImageLocationsForProductAndProductItem = Stream.concat(
        Optional.ofNullable(existingProduct.getProductImages()).orElse(new ArrayList<>()).stream()
          .filter(image -> !image.isMarkForDelete()).filter(ProductImage::isActive).filter(
            productImage -> productImage.getOriginalImage() == null || !productImage.getOriginalImage())
          .map(ProductImage::getLocationPath).collect(Collectors.toSet()).stream(),
        Optional.ofNullable(existingProduct.getProductItems()).orElse(new ArrayList<>()).stream().map(
            productItem -> Optional.ofNullable(productItem.getProductItemImages())
              .orElse(new ArrayList<>())).flatMap(List::stream)
          .filter(image -> !image.isMarkForDelete()).filter(ProductItemImage::isActive).filter(
            productImage -> productImage.getOriginalImage() == null || !productImage.getOriginalImage())
          .map(ProductItemImage::getLocationPath).collect(Collectors.toSet()).stream())
      .collect(Collectors.toSet());
    return activeImageLocationsForProductAndProductItem;
  }

  private static Set<String> getExistingInActiveNonOriginalImages(Product existingProduct) {
    return Stream.concat(
        Optional.ofNullable(existingProduct.getProductImages()).orElse(new ArrayList<>()).stream()
          .filter(Predicate.not(ProductImage::isActive))
          .filter(Predicate.not(ProductImage::isMarkForDelete)).filter(
            productImage -> productImage.getOriginalImage() == null || !productImage.getOriginalImage())
          .map(ProductImage::getLocationPath).collect(Collectors.toSet()).stream(),
        Optional.ofNullable(existingProduct.getProductItems()).orElse(new ArrayList<>()).stream()
          .map(ProductItem::getProductItemImages).filter(Objects::nonNull).flatMap(List::stream)
          .filter(Predicate.not(ProductItemImage::isActive)).filter(
            productImage -> productImage.getOriginalImage() == null || !productImage.getOriginalImage())
          .filter(Predicate.not(ProductItemImage::isMarkForDelete))
          .map(ProductItemImage::getLocationPath).collect(Collectors.toSet()).stream())
      .filter(Objects::nonNull).collect(Collectors.toSet());
  }

  private void validateAndSetBrandCode(Product existingProduct,
    VendorQuickApprovalResponse vendorQuickApprovalResponse) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueByAttributeCodeAndValue = null;
    try {
       predefinedAllowedAttributeValueByAttributeCodeAndValue =
        productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
          brandAttributeCode, existingProduct.getBrand(), false);
    }
    catch (Exception e){
      List<String> errorCodes = new ArrayList<>(vendorQuickApprovalResponse.getErrorCodes());
      errorCodes.add(ApiErrorCode.BRAND_CODE_NOT_ALLOWED.getCode());
      vendorQuickApprovalResponse.setErrorCodes(errorCodes);
      return;
    }
    if (Constants.APPROVED.equalsIgnoreCase(
      predefinedAllowedAttributeValueByAttributeCodeAndValue.getBrandApprovalStatus())
      && Boolean.FALSE.equals(
      predefinedAllowedAttributeValueByAttributeCodeAndValue.isMarkForDelete())) {
      existingProduct.setBrandCode(
        predefinedAllowedAttributeValueByAttributeCodeAndValue.getValue());
    } else {
      List<String> errorCodes = new ArrayList<>(vendorQuickApprovalResponse.getErrorCodes());
      errorCodes.add(ApiErrorCode.BRAND_IS_INREVIEW.getCode());
      vendorQuickApprovalResponse.setErrorCodes(errorCodes);
    }
  }

  private boolean eligibilityForAutoHeal(VendorQuickApprovalResponse vendorQuickApprovalResponse) {
    return vendorQuickApprovalResponse.getErrorCodes().stream()
      .anyMatch(getErrorCodesForAutoHeal()::contains);
  }

  private List<String> getErrorCodesForAutoHeal() {
    return Arrays.asList(ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getCode(),
      ApiErrorCode.PRODUCT_ITEMS_CANNOT_BE_EMPTY.getCode(),
      ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode(),
      ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode());
  }

  private void updateProductDetailsForQuickApproval(Product existingProduct) {
    existingProduct.getProductImages().removeIf(productImage -> Boolean.TRUE.equals(productImage.getOriginalImage()));
    existingProduct.getProductItems().forEach(productItem -> productItem.getProductItemImages()
        .removeIf(productImage -> Boolean.TRUE.equals(productImage.getOriginalImage())));
    if (!existingProduct.isEdited()) {
      this.deleteOriginalImagesForProductAndItems(existingProduct);
    }
    this.productUtils.regenerateProductImageDetails(existingProduct);
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public void updateProductDimensionsAndProductTypeAndDgLevel(
      PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel) throws IOException {
    Product product =
        productRepository.findByProductCodeAndMarkForDeleteFalse(pdtDimensionRefreshEventModel.getProductCode());
    GdnPreconditions.checkArgument(Objects.nonNull(product), PRODUCT_NOT_FOUND_ERROR);
    if (areDimensionsModified(product, pdtDimensionRefreshEventModel)) {
      updateProductNotesForEditedProducts(product, Arrays.asList(Constants.PACKAGE_DIMENSION));
    }
    productUtils.setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    productRepository.save(product);
  }

  private boolean areDimensionsModified(Product product, PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel) {
    if (Objects.nonNull(pdtDimensionRefreshEventModel.getShippingWeight()) && Objects.nonNull(product.getShippingWeight())) {
      if (!product.getWeight().equals(pdtDimensionRefreshEventModel.getWeight())
          || !product.getHeight().equals(pdtDimensionRefreshEventModel.getHeight())
          || !product.getWidth().equals(pdtDimensionRefreshEventModel.getWidth())
          || !product.getLength().equals(pdtDimensionRefreshEventModel.getLength())) {
        return true;
      }
    } else if (Objects.nonNull(pdtDimensionRefreshEventModel.getShippingWeight())
        || Objects.nonNull(product.getShippingWeight())) {
      return true;
    }
    return false;
  }

  @Override
  @Transactional(readOnly = false)
  public PublishAndSavedProductAndHistoryModel autoApproveProduct(String productCode) throws Exception {
    ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(productCode);
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    Product product = productRepository.findByProductCodeAndMarkForDeleteFalse(productCode);
    Pair<Boolean, InternalHistoryEventModel> doPublishAndInternalHistoryModel =
        autoApproveProduct(imageQcProcessedResponseDomainEvent, product, true);
    productRepository.saveAndFlush(product);
    ProductReviewer productReviewer = productReviewerService.findProductReviewerByProductCode(productCode);
    solrVendorCollectionService.updateImageQcResponseToSolr(product, productReviewer);
    publishAutoApprovalEvents(doPublishAndInternalHistoryModel.getLeft(), product);
    return PublishAndSavedProductAndHistoryModel.builder()
        .internalHistoryEventModel(doPublishAndInternalHistoryModel.getRight()).savedProduct(product)
        .doPublish(doPublishAndInternalHistoryModel.getLeft()).build();
  }

  @Override
  public List<Product> getProductsByProductCodes(List<String> productCodes) {
    return productRepository.findByProductCodeInAndMarkForDeleteFalse(productCodes);
  }

  @Override
  public Page<Product> getProductsBySellerCodeAndCategoryCodes(String storeId, String sellerCode, WorkflowState state,
      Set<String> categoryCodes, Pageable pageable) {
    return productRepository.findByStoreIdAndBusinessPartnerCodeAndStateAndCategoryCodeInAndMarkForDeleteFalse(storeId,
        sellerCode, state, categoryCodes, pageable);
  }

  @Override
  public void updateProductRetryStatus(String storeId, String productCode,
    ProductRetryStatusUpdate productRetryStatusUpdate) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
      ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_EMPTY);
    Product product = this.productRepository.findByProductCode(productCode);
    checkState(Objects.nonNull(product), PRODUCT_NOT_FOUND_ERROR);
    product = ConverterUtil.setProductRetryStatus(product, productRetryStatusUpdate);
    Product updatedProduct = this.productRepository.save(product);
    if (StringUtils.isNotEmpty(productRetryStatusUpdate.getState())) {
      List<ProductDistributionTask> productDistributionTaskList =
        this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
          product.getId());
      if (CollectionUtils.isNotEmpty(productDistributionTaskList)) {
        productDistributionTaskList.forEach(
          productDistributionTask -> productDistributionTask.setMarkForDelete(true));
        productDistributionTaskList.get(0)
          .setState(WorkflowState.valueOf(productRetryStatusUpdate.getState()));
        productDistributionTaskList.get(0).setMarkForDelete(false);
      } else {
        productDistributionTaskList =
          distributionTaskService.generateDistributionTaskForProduct(storeId,
            product.getCurrentVendor(), Collections.singletonList(product),
            WorkflowState.valueOf(productRetryStatusUpdate.getState()));
      }
      productDistributionTaskService.saveProductDistributionTaskList(productDistributionTaskList);
    }
    this.solrVendorCollectionService.updateProductOnRetryStatusUpdate(updatedProduct);
  }

  @Override
  public Page<ProductCodeResponse> filterProductWithBoostForAutoAssignment(String storeId,
    String requestId, String username, BoostedProductFilterRequest boostedProductFilterRequest,
    Integer page, Integer size) throws SolrServerException, IOException {
    Pageable pageable = PageRequest.of(page, size);
    try {
      SummaryFilterDTO summaryFilterDTO =
        ConverterUtil.convertBoostedFilterRequestToPrimaryFilterDTO(boostedProductFilterRequest);
      return this.solrVendorCollectionService.getProductResponseForAutoAssignment(storeId,
        summaryFilterDTO, pageable);
    } catch (Exception e) {
      log.error("Error fetching counts from solr For requestId : {} ", requestId, e);
      throw e;
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.REDIS_HIGHER_TTL_MANAGER, value = {
      CacheKeys.VENDOR_AUTO_HEAL}, key = "#keyword", unless = "#result == null")
  public String checkIfVendorAutoHealKeyExists(String keyword) {
    return null;
  }

  @Override
  @Cacheable(cacheManager = Constants.REDIS_HIGHER_TTL_MANAGER, value = {
      CacheKeys.VENDOR_AUTO_HEAL}, key = "#keyword", unless = "#result == null")
  public String cacheVendorAutoHealKey(String keyword) {
    return keyword;
  }

  @Override
  @Transactional
  public Product updateBrandOfProduct(ChangeBrandRequest changeBrandRequest) throws Exception {
    Product product = productRepository.findByProductCode(changeBrandRequest.getProductCode());
    if (Objects.nonNull(product)) {
      product.setBrandCode(changeBrandRequest.getBrandCode());
      product.setBrand(changeBrandRequest.getBrandName());
      if (CollectionUtils.isNotEmpty(product.getProductAttributes())) {
        Optional<ProductAttribute> brand = product.getProductAttributes().stream()
            .filter(productAttribute -> productAttribute.getName().equals(Constants.BRAND)).findFirst();
        brand.ifPresent(productAttribute -> productAttribute.setValue(changeBrandRequest.getBrandName()));
      }
      List<ProductItem> productItems = product.getProductItems();
      productItems.forEach(productItem -> updateProductItems(productItem, changeBrandRequest));
      productRepository.save(product);
      return product;
    }
    return null;
  }

  private void updateProductItems(ProductItem productItem, ChangeBrandRequest changeBrandRequest) {
    Optional<ProductItemAttribute> brand = productItem.getProductItemAttributes().stream()
        .filter(productItemAttribute -> productItemAttribute.getName().equals(Constants.BRAND)).findFirst();
    brand.ifPresent(productItemAttribute -> productItemAttribute.setValue(changeBrandRequest.getBrandName()));
  }

  @Override
  public Page<Product> fetchNeedCorrectionProducts(String storeId, Date startUpdatedDate, Date endUpdatedDate,
      Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(startUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(endUpdatedDate), ErrorMessages.DATE_MUST_NOT_BE_NULL);
    return productRepository.findByStoreIdAndStateAndUpdatedDateBetweenAndMarkForDeleteTrue(storeId,
        WorkflowState.NEED_CORRECTION, startUpdatedDate, endUpdatedDate, pageable);
  }

  @Override
  public void publishInternalHistoryEventForProduct(InternalHistoryEventModel internalHistoryEventModel) {
    log.info("Publishing Internal history event = {} for product = {} ",
        kafkaTopicProperties.getInternalHistoryEventName(), internalHistoryEventModel.getProductCode());
    kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(), internalHistoryEventModel.getProductCode(),
        internalHistoryEventModel);
  }

  @Override
  public AppealProductResponse updateAppealProduct(AppealProductRequest appealProductRequest,
    String storeId) throws Exception {
    Product product = productRepository.findByProductCode(appealProductRequest.getProductCode());
    if (Objects.isNull(product)) {
      return AppealProductResponse.builder().errorMessage(ApiErrorCode.PRODUCT_NOT_FOUND.getDesc())
        .errorCode(ApiErrorCode.PRODUCT_NOT_FOUND.getCode()).build();
    }
    if (!APPEALED_PRODUCT_STATES.contains(product.getState())) {
      return AppealProductResponse.builder()
        .errorMessage(ApiErrorCode.PRODUCT_IS_IN_INVALID_STATE.getDesc())
        .errorCode(ApiErrorCode.PRODUCT_IS_IN_INVALID_STATE.getCode()).build();
    }
    product.setAppealedProduct(true);
    this.updateProduct(product);
    AppealedProduct appealedProduct = Optional.ofNullable(
        appealProductService.findAppealProductByProductCode(product.getProductCode())).orElseGet(
        () -> AppealedProduct.builder().productCode(product.getProductCode())
            .businessPartnerCode(product.getBusinessPartnerCode()).build());
    appealedProduct.setStoreId(storeId);
    appealedProduct.setAppealedProductNotes(appealProductRequest.getNotes());
    appealProductService.upsertAppealProduct(appealedProduct);
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
            VendorProductSolrHelper.toProductUpdateProductToSolrEventModelForBrandUpdate(product);
    pdtProductUpdateProductToSolrEventModel.setAppealedProduct(true);
    solrReindexPublisherService.publishPDTProductApprovalToSolr(pdtProductUpdateProductToSolrEventModel);
    return AppealProductResponse.builder().build();
  }
}
