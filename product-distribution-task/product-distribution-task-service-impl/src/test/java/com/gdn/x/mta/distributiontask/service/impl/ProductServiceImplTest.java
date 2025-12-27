package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.dao.api.feign.XBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XInventoryFeign;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import com.gdn.x.mta.distributiontask.model.dto.L2StockDetailResponse;
import com.gdn.x.mta.distributiontask.request.AppealProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.AppealProductResponse;
import com.gdn.x.mta.distributiontask.service.api.AppealProductService;
import com.gdn.x.mta.distributiontask.service.api.GcsService;
import com.gdn.x.mta.distributiontask.service.impl.config.GcsProperties;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.common.SolrException;
import org.aspectj.util.Reflection;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.ItemNeedRevisionNotes;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.config.BeanConfiguration;
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
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductRejectedEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
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
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.ProductHistoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.dto.StuckProductsDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.enums.ApiErrorCode;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.type.BrandApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.NeedRevisionType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PublishAndSavedProductAndHistoryModel;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.QuickApprovalResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ErrorMessages;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
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
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.util.ImageUtils;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;
import com.gdn.x.mta.distributiontask.service.impl.util.VendorUtils;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.util.NotesAndRejectReason;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.OldAndNewPathDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.VideoListResponse;
import com.google.common.collect.ImmutableSet;

import javax.imageio.ImageIO;

public class ProductServiceImplTest {

  public static final String CODE = "code";
  private static final String STORE_ID = "10001";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final int ASSIGNEE_LIST_SIZE = 1;
  private final static String PRODUCT_NAME1 = "NAME1";
  private final static String PRODUCT_NAME2 = "NAME2";
  private final static String PRODUCT_CODE = "CODE";
  private final static String PRODUCT_ID = "ID";
  private final static String BUSINESS_PARTNER_CODE = "BP_CODE";
  private final static String BUSINESS_PARTNER_NAME = "BP_NAME";
  private final static String CATEGORY_CODE = "CAT_CODE";
  private final static String VENDOR_CODE = "VEND_CODE";
  private final static String VENDOR_CODE1 = "VEND_CODE1";
  private final static String VENDOR_ID = "VEND_ID";
  private static final String PRODUCT_CODE2 = "CODE2";
  private static final String TASK_CODE = "TASK_CODE1";
  private static final String PRODUCT_ID2 = "ID2";
  private static final String VENDOR_NAME = "VENDOR_NAME";
  private static final String DEFAULT_SORT_METHOD = "asc";
  private static final String USERNAME = "username";
  private static final String DEFAULT_USERNAME = "System";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String SKU_CODE = "skuCode";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String REJECT_REASON = "rejectReasonRequest";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String FINAL_NOTES = "Content Approved by Vendor null-additionalNotes";
  private static final String ACTION_TYPE = "sendForRevision";
  private final PrimaryFilterDTO primaryFilterDTO =
      new PrimaryFilterDTO().builder().contentPending(Boolean.TRUE).imagePending(Boolean.TRUE).keyword(DEFAULT_KEYWORD)
          .timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE).brandPending(Boolean.TRUE).build();
  private final Pageable DEFAULT_PAGEABLE  = PageRequest.of(PAGE, SIZE);
  private static final String DEFAULT_KEYWORD = "keyword";
  private static final String DEFAULT_ASSIGNEE_KEYWORD = "ssigne";
  private static final WorkflowState IN_REVIEW_STATE = WorkflowState.IN_REVIEW;
  private static final String TODAY = "today";
  private static final String YESTERDAY = "yesterday";
  private static final String TWO_DAYS_AGO = "twoDaysAgo";
  private static final String THREE_UNTIL_FIVE_DAYS_AGO = "threeUntilFiveDaysAgo";
  private static final String MORE_THAN_5_DAYS = "moreThan5Days";
  private static final String PENDING = "pending";
  private static final String CONTENT_PENDING = "contentPending";
  private static final String IMAGE_PENDING = "imagePending";
  private static final String ASSIGNED = "assigned";
  private static final String UNASSIGNED = "unassigned";
  private static final String BRAND_NOT_APPROVED = "brandNotApproved";
  private static final String EMPTY_LIST_ELEMENT = "NA";
  private static final String PRODUCT_ITEM_ID1 = "ID1";
  private static final String PRODUCT_ITEM_ID2 = "ID2";
  private static final List<String> PRODUCT_IDS = new ArrayList<>();
  private static final List<String> PRODUCT_ITEM_IDS = new ArrayList<>();
  private static final int BATCH_SIZE = 1;
  private static final String VALUE = "val";
  private static final String VALUE2 = "val2";
  private static final String INTERNAL = "INTERNAL";
  private static final String RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID = "productStuckAlert";
  private static final String EMAIL_ADDRESS = "email@gmail.com";
  private static final String EMAIL_OBJECT = "obj";
  private static final String IMAGE_VIOLATION = "Blur";
  private static final String IMAGE_QC_RESPONSE = "qcResponse";
  private static final String BRAND_STATUS = "brandPending";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String PREDICTION_TYPE_1 = "watermark";
  private static final long VERSION = 10;
  private static final String NOT_ACTIVE = "NOT_ACTIVE";
  private static final String TEST_ASSIGNEE = "TEST_ASSIGNEE";
  private static final String MODIFIED_FIELD = "MODIFIED_FIELD";
  private static final String NOTES = "NOTES";
  private static final String ATTRIBUTE_CODE1 = "ATTRIBUTE_CODE1";
  private static final String ATTRIBUTE_CODE2 = "ATTRIBUTE_CODE2";
  private static final String ATTRIBUTE_CODE3 = "ATTRIBUTE_CODE3";
  private static final String ATTRIBUTE_CODE4 = "ATTRIBUTE_CODE4";
  private static final String ATTRIBUTE_CODE5 = "ATTRIBUTE_CODE5";
  private static final String ATTRIBUTE_CODE9 = "ATTRIBUTE_CODE9";
  private static final String ATTRIBUTE_CODE10 = "ATTRIBUTE_CODE10";
  private static final String ATTRIBUTE_VALUE1 = "ATTRIBUTE_VALUE1";
  private static final String ATTRIBUTE_VALUE2 = "ATTRIBUTE_VALUE2";
  private static final String ATTRIBUTE_VALUE3 = "ATTRIBUTE_VALUE3";
  private static final String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  private static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";
  private static final String DESCRIPTIVE_ATTRIBUTE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String DESCRIPTION = "DESCRIPTION";

  private static Date DEFAULT_END_DATE = new Date();
  private static final Calendar calendar = Calendar.getInstance();
  private static final Date DEFAULT_START_DATE = new Date(0);
  private final List<WorkflowState> STATES = new ArrayList<>();
  private ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse =
      new ProductBusinessPartnerMapperResponse();
  private final List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList = new ArrayList<>();
  private static final String DEFAULT_ASSIGNEE_EMAIL_ID = "assigneeEmailId";
  private final List<String> assigneeResponseList = new ArrayList<>();
  private static final String PRODUCT_CODE1 = "PRODUCT_CODE";
  private static final List<String> PRODUCT_CODES = new ArrayList<>();
  private static final String ASSIGNED_BY = " assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGN = "assign";
  private static final String UNASSIGN = "unassign";
  private static final Product product1 = new Product();
  private static final String ID1 = "ID1";
  private static final String ID = "ID";
  private static final String CATEGORY_NAME = "CAT_NAME";
  private static final String ASSIGNED_BY_TEXT = " Assigned By ";
  private static final String UPDATE_HISTORY_NOTES = "Diubah : [{\"skuName\":null,\"field\":\"Video URL\","
      + "\"oldValue\":\"val\",\"newValue\":\"val2\"}]";
  private static final String ACTIVE_STATE = "ACTIVE";
  private static final String IN_VENDOR_STATE = "IN_VENDOR";
  private static final String LOCATION_PATH = "locationPath";
  private static final boolean MAIN_IMAGE = true;
  private static final int SEQUENCE = 0;
  private static final String IMAGE_SOURCE_DIRECTORY = "imageSourceDirectory";
  private static final boolean MARK_FOR_DELETE = false;
  private static final String IMAGE_LOCATION = "location";
  private static final String ROOT = "/";
  private static final String APPROVAL_STATE = "APPROVAL";
  private static final String BASE_FOLDER = "src/test/testFolders";
  private static final String FILE_NAME1 = "Testing1.txt";
  private static final String FILE_NAME2 = "Testing2.txt";
  private static final String FILE_NAME_PATH1 = "1/CODE2/Testing1.txt";
  private static final String FILE_NAME_PATH2 = "1/CODE2/Testing2.txt";
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_URL2 = "https://www.youtube.com/watch?v=IEordePs-70";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String INVALID_URL = "video url is invalid";
  private static final String RESTRICTED_KEYWORDS_FIELD =
      "[{\"fieldIdentifier\" : \"PRODUCT_NAME\", \"keywords\" : [\"abc@gmail.com\"]}]";
  private static final Integer MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE = 10;
  private static final Integer MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE_ZERO = 0;
  private static final String REASON = "REASON";
  private static final String STATE = "IMAGE_NEED_CORRECTION";
  private static final String ASSIGNED_REASON = "Reviewer Assigned to ";
  private static final String PRODUCT = "PRODUCT";
  public static final String ASSIGNEE = "Assignee";
  public static final Double VALUE_1 = 12.0;
  public static final Double VALUE_2 = 13.0;
  public static final String OLD_PATH1 = "oldPath1";
  public static final String OLD_PATH2 = "oldPath2";
  public static final String NEW_PATH1 = "newPath1";
  public static final String NEW_PATH2 = "newPath2";
  public static final String PRODUCT_CODE_1 = "PRODUCT_CODE";
  public static final String PRODUCT_SKU = "PRODUCT_SKU";
  public static final String CC = "CC";

  private final List<Product> products = new ArrayList<>();
  private Object[] primaryFilterResponse;
  private Object[] reviewProductCountResponse;
  private final List<String> primaryFilterValues = new ArrayList<>(Arrays
      .asList(TODAY, YESTERDAY, TWO_DAYS_AGO, THREE_UNTIL_FIVE_DAYS_AGO, MORE_THAN_5_DAYS, PENDING, CONTENT_PENDING,
          IMAGE_PENDING, ASSIGNED, UNASSIGNED, BRAND_NOT_APPROVED));
  private final List<String> categoryCodes = new ArrayList<>();
  private final List<Product> productList = new ArrayList<>();
  private Page<Product> productPage1;
  private Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOS;
  private SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
  private CategoryCodeResponse categoryCodeResponse;
  private GdnRestSingleResponse<CategoryCodeResponse> categoryCodeResponseGdnRestListResponse;
  private List<ProductHistoryDTO> productHistoryDTOS;
  private final ObjectMapper objectMapper = new ObjectMapper();
  private final Date todayStart = DateUtils.addMilliseconds(DateUtils.ceiling(new Date(), Calendar.DATE), 0);
  Map<String, Boolean> status = new HashMap<>();
  private ProductWorkflowStatusResponse productWorkflowStatus;
  private Product VendorProduct;
  private List<ProductImage> productImages;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;
  private ImageQcProcessedResponseDomainEvent imageQcProcessedResponseDomainEvent;
  private BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel;
  private ProductSystemParameterResponse productSystemParameterResponse;
  private Map<String, Object> countsMap;
  private AutoNeedRevisionRequest autoNeedRevisionRequest;
  private RetryAutoNeedRevisionResponse autoNeedRevisionResponse;
  private final Product product2 = new Product();
  private final PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel2 = new PDTDimensionRefreshEventModel();
  private final ProductRetryStatusUpdate productRetryStatusUpdate = new ProductRetryStatusUpdate();
  private List<ProductImage> deleteProductImages;
  private List<ProductItemImage> deleteProductItemImages;
  private ImagePathUpdateDomainEventModel imagePathUpdateDomainEventModel;
  private OldAndNewPathDomainEventModel oldAndNewPathDomainEventModel1;
  private OldAndNewPathDomainEventModel oldAndNewPathDomainEventModel2;
  private ProfileResponse profileResponse;
  private L2StockDetailResponse l2StockDetailResponse;

  @InjectMocks private ProductServiceImpl instance;

  @Mock private ProductRepository productRepository;

  @Mock private ProductPublisherService productPublisherService;

  @Mock private ProductImageRepository productImageRepository;

  @Mock private ProductItemRepository productItemRepository;

  @Mock private ProductAttributeRepository productAttributeRespository;

  @Mock private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock private ProductUtils productUtils;

  @Mock private ProductDistributionTaskService productDistributionTaskService;

  @Mock private VendorRepository vendorRepository;

  @Mock private TaskHistoryService taskHistoryService;

  @Mock private TaskHistoryRepository taskHistoryRepository;

  @Mock private ApprovedProductPublisherService approvedProductPublisherService;

  @Mock private VendorUtils vendorUtils;

  @Mock private Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers;

  @Mock private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Mock private ProductItemAttributeRepository productItemAttributeRepository;

  @Mock private SolrVendorCollectionService solrVendorCollectionService;

  @Mock Pageable pageable;

  @Mock private YouTube youTube;

  @Mock private YouTube.Videos videos;

  @Mock private YouTube.Videos.List list;
  private VideoListResponse videoListResponse;

  @Mock
  private DistributionTaskService distributionTaskService;

  @Mock
  private ProductItemImageRepository productItemImageRepository;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private ProductWorkflowRepository productWorkflowRepository;

  @Mock
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private XInventoryFeign xInventoryFeign;

  @Mock
  private XBPFeign xBPFeign;

  @Mock
  private ObjectMapper mapper;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private AppealProductService appealProductService;

  @Mock
  private SolrReindexPublisherService solrReindexPublisherService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Captor
  private ArgumentCaptor<Product> productCaptor;

  @Captor
  private ArgumentCaptor<List<Product>> productListCaptor;

  @Captor
  private ArgumentCaptor<Object> objectArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDistributionTask> productDistributionTaskArgumentCaptor;

  @Captor
  private ArgumentCaptor<WorkflowState> workflowStateArgumentCaptor;

  @Captor
  private ArgumentCaptor<MessageEmailRequest> messageEmailRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcFeedbackRequest> productImageQcFeedbackRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductHistoryRequest> productHistoryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAutoApproval> productAutoApprovalArgumentCaptor;

  private ArgumentCaptor<List> listArgumentCaptor = null;
  private ArgumentCaptor<Product> productArgumentCaptor = null;

  @Captor
  private ArgumentCaptor<List<TaskHistory>> taskHistoryArgumentCaptorList;

  @Captor
  private ArgumentCaptor<TaskHistory> taskHistoryArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductReviewer> productReviewerArgumentCaptor;

  @Mock
  private ProductActionRetryService productActionRetryService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private GcsService gcsService;

  @Mock
  private GcsProperties gcsProperties;

  private Product product;

  private Product oldProduct;

  private Product newProduct;

  private Vendor vendor;

  private Page<Product> productPage;

  private List<WorkflowState> workflowStateList;

  private ProductRejectedEventModel productRejectedEventModel;

  private ScreeningProductBulkActionsRequest request;
  private NeedRevisionRequest needRevisionRequest;
  private BulkVendorProductActionsDTO bulkVendorProductActionsDTO;
  private BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO;
  private ProductDistributionTask productDistributionTask;
  private RejectProductDTO rejectProductDTO;
  private ProductWorkflowStatusResponse productWorkflowStatusResponse;
  private File file1;
  private File file2;
  private final PageRequest pageRequest1 = PageRequest.of(0, BATCH_SIZE *   2);
  private final PageRequest pageRequest2 = PageRequest.of(0, 1);
  private final List<ProductAutoApproval> productAutoApprovalList = new ArrayList<>();
  private final List<ProductActionRetry> productActionRetryList = new ArrayList<>();
  private RetryAutoNeedRevisionResponse retryAutoNeedRevisionResponse;
  private ProductReviewer productReviewer = new ProductReviewer();
  private List<ProductReviewer> productReviewers;
  private List<String> modifiedFields;
  private VendorQuickApprovalResponse vendorQuickApprovalResponse;
  private CategoryDetailResponse categoryDetailResponse;
  private final boolean isQuickApproval = false;
  private static final String TOPIC = "topic"; 

  private List<Product> createProductList() {
    Vendor vendor =
        new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    Product product = new Product.Builder().id(PRODUCT_ID).productName("productName").productCode("code")
        .productAttributes(new ArrayList<>()).currentVendor(vendor).productImages(new ArrayList<>())
        .productItems(new ArrayList<>()).build();
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    product = new Product.Builder().id(PRODUCT_ID).productName("productNameChanged").productCode("code")
        .productAttributes(new ArrayList<>()).currentVendor(vendor).productImages(new ArrayList<>())
        .productItems(new ArrayList<>()).build();
    product.setVideoUrl(YOUTUBE_URL2);
    productList.add(product);
    return productList;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MockitoAnnotations.openMocks(this);
    String hash = "123";
    ReflectionTestUtils.setField(instance, "validateProductReject", true);
    ReflectionTestUtils.setField(instance, "qcProductsRetryCount", 4);
    this.product = new Product();
    this.product.setId(PRODUCT_ID);
    this.product.setProductCode(PRODUCT_CODE);
    this.product.setState(WorkflowState.IN_REVIEW);
    this.productRejectedEventModel = new ProductRejectedEventModel();

    l2StockDetailResponse = new L2StockDetailResponse();
    l2StockDetailResponse.setDistributionWarehouseAvailable(false);
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(false);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setMerchantType(CC);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    profileResponse.setCompany(companyDTO);

    product2.setHeight(VALUE_1);
    product2.setWeight(VALUE_1);
    product2.setWidth(VALUE_1);
    product2.setLength(VALUE_1);
    product2.setShippingWeight(VALUE_1);

    pdtDimensionRefreshEventModel2.setProductCode(PRODUCT_CODE);
    pdtDimensionRefreshEventModel2.setHeight(VALUE_2);
    pdtDimensionRefreshEventModel2.setWeight(VALUE_2);
    pdtDimensionRefreshEventModel2.setWidth(VALUE_2);
    pdtDimensionRefreshEventModel2.setLength(VALUE_2);
    pdtDimensionRefreshEventModel2.setShippingWeight(VALUE_2);


    this.oldProduct = new Product();
    this.oldProduct.setId(PRODUCT_ID);
    this.oldProduct.setProductName(PRODUCT_NAME1);
    this.oldProduct.setProductCode(PRODUCT_CODE);
    this.oldProduct.setState(WorkflowState.REJECTED);
    this.oldProduct.setProductImages(new ArrayList<ProductImage>());
    this.oldProduct.setProductAttributes(new ArrayList<ProductAttribute>());
    this.oldProduct.setProductItems(new ArrayList<ProductItem>());
    this.oldProduct.getProductImages().add(new ProductImage(this.oldProduct, "location", 0, true));
    this.oldProduct.getProductImages().get(0).setOriginalImage(true);
    this.oldProduct.getProductItems()
        .add(new ProductItem(this.oldProduct, "upcCode", "skuCode", "generatedItemName", hash.getBytes(), "STORE_ID"));
    this.oldProduct.getProductAttributes()
        .add(new ProductAttribute(this.oldProduct, "attributeCode", "name", "value", "attributeType"));
    this.oldProduct.setRestrictedKeywordsDetected(RESTRICTED_KEYWORDS_FIELD);
    this.oldProduct.setImageViolations(IMAGE_VIOLATION);
    this.oldProduct.setTextViolations("Blur");

    this.newProduct = new Product();
    this.newProduct.setId(PRODUCT_ID);
    this.newProduct.setProductCode(PRODUCT_CODE);
    this.newProduct.setProductName(PRODUCT_NAME2);
    this.newProduct.setState(WorkflowState.REJECTED);
    this.newProduct.setProductImages(new ArrayList<ProductImage>());
    this.newProduct.setProductAttributes(new ArrayList<ProductAttribute>());
    this.newProduct.setProductItems(new ArrayList<ProductItem>());
    this.newProduct.getProductImages().add(new ProductImage(this.newProduct, "location", 0, true));
    this.newProduct.getProductItems()
        .add(new ProductItem(this.newProduct, "upcCode", "skuCode", "generatedItemName", hash.getBytes(), "STORE_ID"));
    this.newProduct.getProductAttributes()
        .add(new ProductAttribute(this.newProduct, "attributeCode", "name", "value", "attributeType"));
    this.newProduct.setBrand(BRAND_NAME);
    this.newProduct.setBrandCode(BRAND_CODE);
    this.newProduct.setImageViolations(IMAGE_VIOLATION);
    this.newProduct.setTextViolations("Blur");
    this.vendor =
        new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    this.productPage = new PageImpl<Product>(createProductList());
    this.workflowStateList = new ArrayList<>();
    this.workflowStateList.add(WorkflowState.PASSED);
    productBusinessPartnerMapperResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productBusinessPartnerMapperResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    calendar.add(Calendar.SECOND, 1);
    DEFAULT_END_DATE = calendar.getTime();
    assigneeResponseList.add(DEFAULT_ASSIGNEE_EMAIL_ID);

    productReviewer = new ProductReviewer();
    productReviewer.setProductCode(PRODUCT_CODE);
    productReviewer.setApproverAssignee(DEFAULT_USERNAME);

    categoryCodes.add(CATEGORY_CODE);
    product1.setCategoryCode(CATEGORY_CODE);
    product1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    product1.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    product1.setStoreId(STORE_ID);
    productList.add(product1);
    productPage1 = new PageImpl<>(productList, PageRequest.of(PAGE, SIZE), productList.size());
    productAndReviewerDetailsDTOS =
        new PageImpl<>(List.of(new ProductAndReviewerDetailsDTO(product1, productReviewer)),
            PageRequest.of(PAGE, SIZE), productList.size());
    summaryFilterDTO =
        SummaryFilterDTO.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).assigneeEmailId(DEFAULT_ASSIGNEE_EMAIL_ID)
            .sortOrderByCreatedDate(DEFAULT_SORT_METHOD).categoryCode(CATEGORY_CODE).contentPending(Boolean.TRUE)
            .imagePending(Boolean.TRUE).timeFilterType(TimeFilterType.ALL).keyword(DEFAULT_KEYWORD)
            .isCnCategory(Boolean.TRUE).vendorCode(VENDOR_CODE).faultyImageType(IMAGE_VIOLATION)
            .brandPending(Boolean.TRUE).build();

    categoryCodeResponse = new CategoryCodeResponse(categoryCodes);
    categoryCodeResponseGdnRestListResponse = new GdnRestSingleResponse<>(categoryCodeResponse, REQUEST_ID);

    PRODUCT_CODES.add(PRODUCT_CODE1);
    product1.setId(ID1);
    product1.setProductCode(PRODUCT_CODE1);
    product1.setProductName(PRODUCT_NAME1);
    product1.setCategoryCode(CATEGORY_CODE);
    product1.setCategoryName(CATEGORY_NAME);
    product1.setCurrentVendor(vendor);
    product1.setState(WorkflowState.IN_REVIEW);
    products.add(product1);
    PRODUCT_IDS.add(ID1);
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID1);
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID2);
    primaryFilterResponse = new Object[primaryFilterValues.size()];
    setPrimaryFilterResponse();
    reviewProductCountResponse = new Object[2];
    reviewProductCountResponse[0] = 10;
    reviewProductCountResponse[1] = 10;
    listArgumentCaptor = ArgumentCaptor.forClass(List.class);
    productArgumentCaptor = ArgumentCaptor.forClass(Product.class);

    request = new ScreeningProductBulkActionsRequest();
    request.setProductCodes(new ArrayList<>(List.of(PRODUCT_CODE)));
    request.setAdditionalNotes(ADDITIONAL_NOTES);
    request.setCorrectionReason(CORRECTION_REASON);
    request.setRejectionReason(REJECT_REASON);
    request.setVendorNotes(Arrays.asList(CORRECTION_REASON, ADDITIONAL_NOTES));
    request.setAllVariants(true);
    request.setContentAdditionalNotes(ADDITIONAL_NOTES);
    request.setItemNotes(List.of(
        new ItemNeedRevisionNotes(PRODUCT_ITEM_ID1, PRODUCT_CODE, 1, PRODUCT_NAME1,
            List.of(CORRECTION_REASON), Arrays.asList(CORRECTION_REASON, ADDITIONAL_NOTES))));

    needRevisionRequest = new NeedRevisionRequest();
    needRevisionRequest.setStoreId(STORE_ID);
    needRevisionRequest.setProductCodes(new ArrayList<>(List.of(PRODUCT_CODE)));
    needRevisionRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    needRevisionRequest.setCorrectionReason(CORRECTION_REASON);
    needRevisionRequest.setRejectionReason(REJECT_REASON);
    ProductNotesRequest productNotesRequest =
        ProductNotesRequest.builder().contentAdditionalNotes("test").allVariants(true).build();
    needRevisionRequest.setProductNotesRequest(productNotesRequest);

    productHistoryDTOS = Collections.singletonList(
        ProductHistoryDTO.builder().oldValue(VALUE).newValue(VALUE2).field(Constants.PRODUCT_URL).build());

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(LOCATION_PATH);
    productImage.setMainImage(MAIN_IMAGE);
    productImage.setSequence(SEQUENCE);
    productImage.setMarkForDelete(MARK_FOR_DELETE);
    productImages = new ArrayList<>();
    productImages.add(productImage);

    Mockito.when(productUtils.getProductDetailChanges(this.oldProduct, this.newProduct)).thenReturn(productHistoryDTOS);
    Mockito.when(productUtils.toJson(productHistoryDTOS))
        .thenReturn(objectMapper.writeValueAsString(productHistoryDTOS));
    Mockito.when(productRepository.save(this.oldProduct)).thenReturn(this.oldProduct);
    Mockito.when(productUtils.replaceProductDetails(this.oldProduct, this.newProduct, true))
        .thenReturn(this.oldProduct);
    Mockito.when(productUtils.replaceProductDetails(this.oldProduct, this.newProduct, false))
        .thenReturn(this.oldProduct);
    Mockito.when(productUtils.getImageChanges(this.oldProduct, this.newProduct)).thenReturn(productHistoryDTOS);
    Mockito.when(productUtils.replaceProductImageDetails(this.oldProduct, this.newProduct)).thenReturn(this.oldProduct);
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(this.oldProduct, this.newProduct))
        .thenReturn(this.oldProduct);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(Mockito.eq(ID1))).thenReturn(TASK_CODE);

    status = new HashMap<>();
    status.put(IN_VENDOR_STATE, true);
    productWorkflowStatus =
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<>(Collections.singletonList(IN_VENDOR_STATE)),
            status);

    VendorProduct = new Product();

    bulkVendorProductActionsDTO = new BulkVendorProductActionsDTO();
    bulkScreeningProductActionsDTO = new BulkScreeningProductActionsDTO();
    bulkScreeningProductActionsDTO.setProductCodes(PRODUCT_CODES);
    bulkScreeningProductActionsDTO.setAssignedBy(ASSIGNED_BY);
    bulkScreeningProductActionsDTO.setAssignTo(ASSIGNED_TO);
    bulkVendorProductActionsDTO.setBulkScreeningProductActionsRequests(
        Collections.singletonList(bulkScreeningProductActionsDTO));

    ReflectionTestUtils.setField(instance, "sendStuckProductSummaryEmailAddressCc", EMAIL_ADDRESS);

    STATES.add(IN_REVIEW_STATE);
    STATES.add(WorkflowState.NEED_CORRECTION);

    productDistributionTask = new ProductDistributionTask();
    productDistributionTask.setTaskCode(TASK_CODE);
    productDistributionTask.setProduct(this.newProduct);
    productDistributionTask.setVendor(vendor);

    productWorkflowStatusResponse = new ProductWorkflowStatusResponse();
    productWorkflowStatusResponse.setStates(List.of(APPROVAL_STATE));

    rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode(PRODUCT_CODE);
    rejectProductDTO.setNotes(NOTES);
    RejectReasonDto rejectReasonDto = new RejectReasonDto();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReasonDto.setProduct(product);
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);

    imageQcProcessedResponseDomainEvent = new ImageQcProcessedResponseDomainEvent();
    imageQcProcessedResponseDomainEvent.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponseDomainEvent.setImageQcResponse(IMAGE_QC_RESPONSE);
    imageQcProcessedResponseDomainEvent.setImageViolations(IMAGE_VIOLATION);
    imageQcProcessedResponseDomainEvent.setProductPredictionScore(10);

    brandApprovedOrRejectedDomainEventModel = new BrandApprovedOrRejectedDomainEventModel();
    brandApprovedOrRejectedDomainEventModel.setBrandName(BRAND_NAME);
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BRAND_STATUS);
    brandApprovedOrRejectedDomainEventModel.setBrandRequestCode(BRAND_REQUEST_CODE);

    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);

    productSystemParameterResponse = new ProductSystemParameterResponse();
    productSystemParameterResponse.setVariable(Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    productSystemParameterResponse.setValue("true");

    countsMap = new HashMap<>();

    ProductAutoApproval productAutoApproval = new ProductAutoApproval();
    productAutoApproval.setProductCode(PRODUCT_CODE);
    productAutoApprovalList.add(productAutoApproval);
    autoNeedRevisionRequest = new AutoNeedRevisionRequest();
    autoNeedRevisionRequest.setProductCode(PRODUCT_CODE);
    autoNeedRevisionRequest.setReason(REASON);
    autoNeedRevisionRequest.setState(STATE);
    autoNeedRevisionResponse = new RetryAutoNeedRevisionResponse();
    autoNeedRevisionResponse.setState(WorkflowState.PASSED.toString());
    autoNeedRevisionRequest.setReason(REASON);
    ProductActionRetry productActionRetry = new ProductActionRetry();
    productActionRetry.setProductCode(PRODUCT_CODE);
    productActionRetry.setAction(ACTIVE_STATE);
    productActionRetryList.add(productActionRetry);
    retryAutoNeedRevisionResponse = new RetryAutoNeedRevisionResponse();
    retryAutoNeedRevisionResponse.setProductCode(PRODUCT_CODE);
    retryAutoNeedRevisionResponse.setProductActive(true);
    retryAutoNeedRevisionResponse.setState(WorkflowState.PASSED.toString());
    ImageUtils.setFileStorageService(fileStorageService);

    productReviewers = new ArrayList<>();
    productReviewers.add(productReviewer);
    modifiedFields = new ArrayList<>();
    modifiedFields.add(MODIFIED_FIELD);

    CategoryAttributeResponse productAttributeResponse1 = new CategoryAttributeResponse();
    productAttributeResponse1.setMarkForDelete(false);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    attributeResponse1.setAttributeType(DEFINING_ATTRIBUTE);
    productAttributeResponse1.setAttribute(attributeResponse1);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValue = new AllowedAttributeValueResponse();
    allowedAttributeValue.setAllowedAttributeCode(ATTRIBUTE_CODE1);
    allowedAttributeValue.setValue(ATTRIBUTE_VALUE1);
    productAttributeValueResponse1.setAllowedAttributeValue(allowedAttributeValue);
    productAttributeValueResponse1.setMarkForDelete(false);

    CategoryAttributeResponse productAttributeResponse2 = new CategoryAttributeResponse();
    productAttributeResponse2.setMarkForDelete(false);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE2);
    attributeResponse2.setAttributeType(DESCRIPTIVE_ATTRIBUTE);
    productAttributeResponse2.setAttribute(attributeResponse2);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setDescriptiveAttributeValue(DESCRIPTION);
    productAttributeValueResponse2.setMarkForDelete(false);

    CategoryAttributeResponse productAttributeResponse3 = new CategoryAttributeResponse();
    productAttributeResponse3.setMarkForDelete(false);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE3);
    attributeResponse3.setAttributeType(PREDEFINED_ATTRIBUTE);
    productAttributeResponse3.setAttribute(attributeResponse3);
    ProductAttributeValueResponse productAttributeValueResponse3 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE3);
    predefinedAllowedAttributeValueResponse.setValue(ATTRIBUTE_VALUE2);
    productAttributeValueResponse3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse3.setMarkForDelete(false);

    CategoryAttributeResponse productAttributeResponse4 = new CategoryAttributeResponse();
    productAttributeResponse4.setMarkForDelete(false);
    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE4);
    productAttributeResponse4.setAttribute(attributeResponse4);
    ProductAttributeValueResponse productAttributeValueResponse4 = new ProductAttributeValueResponse();
    productAttributeValueResponse4.setMarkForDelete(false);
    productAttributeValueResponse4.setPredefinedAllowedAttributeValue(null);

    CategoryAttributeResponse productAttributeResponse5 = new CategoryAttributeResponse();
    productAttributeResponse5.setMarkForDelete(false);
    AttributeResponse attributeResponse5 = new AttributeResponse();
    attributeResponse5.setAttributeCode(ATTRIBUTE_CODE5);
    attributeResponse5.setAttributeType(DEFINING_ATTRIBUTE);
    productAttributeResponse5.setAttribute(attributeResponse5);
    ProductAttributeValueResponse productAttributeValueResponse5 = new ProductAttributeValueResponse();
    productAttributeValueResponse5.setMarkForDelete(true);
    AllowedAttributeValueResponse allowedAttributeValue2 = new AllowedAttributeValueResponse();
    allowedAttributeValue2.setAllowedAttributeCode(ATTRIBUTE_CODE5);
    allowedAttributeValue2.setValue(ATTRIBUTE_VALUE3);
    productAttributeValueResponse5.setAllowedAttributeValue(allowedAttributeValue2);

    CategoryAttributeResponse productAttributeResponse6 = new CategoryAttributeResponse();
    productAttributeResponse6.setMarkForDelete(false);
    AttributeResponse attributeResponse6 = new AttributeResponse();
    attributeResponse6.setAttributeCode(ATTRIBUTE_CODE9);
    attributeResponse6.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse6.setAttribute(attributeResponse6);
    ProductAttributeValueResponse productAttributeValueResponse6 = new ProductAttributeValueResponse();
    productAttributeValueResponse6.setMarkForDelete(false);
    productAttributeValueResponse6.setPredefinedAllowedAttributeValue(null);

    CategoryAttributeResponse productAttributeResponse7 = new CategoryAttributeResponse();
    productAttributeResponse7.setMarkForDelete(false);
    AttributeResponse attributeResponse7 = new AttributeResponse();
    attributeResponse7.setAttributeCode(ATTRIBUTE_CODE10);
    attributeResponse7.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributeResponse7.setAttribute(attributeResponse7);
    ProductAttributeValueResponse productAttributeValueResponse7 = new ProductAttributeValueResponse();
    productAttributeValueResponse7.setDescriptiveAttributeValue(null);
    productAttributeValueResponse7.setMarkForDelete(false);

    categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(
        Arrays.asList(productAttributeResponse1, productAttributeResponse2, productAttributeResponse3,
            productAttributeResponse4, productAttributeResponse5, productAttributeResponse6,
            productAttributeResponse7));
    vendorQuickApprovalResponse = VendorQuickApprovalResponse.builder().build();
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(productReviewer);
    Mockito.when(productUtils.validateProtectedBrand(Mockito.any(), Mockito.any())).thenReturn(true);

    productRetryStatusUpdate.setEdited(Boolean.TRUE);
    productRetryStatusUpdate.setRevised(Boolean.TRUE);

    Mockito.when(this.productUtils.getBrandCodeByBrandName(Mockito.anyString())).thenReturn(BRAND_CODE);

    ProductImage productImage1 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItemImage productItemImage2 = new ProductItemImage();
    productImage2.setOriginalImage(true);
    productItemImage2.setOriginalImage(true);
    deleteProductImages = Arrays.asList(productImage1, productImage2);
    deleteProductItemImages = Arrays.asList(productItemImage1, productItemImage2);

    Mockito.when(productImageRepository.findByProductId(Mockito.anyString())).thenReturn(productImages);
    Mockito.when(productItemImageRepository.findByProductId(Mockito.anyString())).thenReturn(
        List.of());

    imagePathUpdateDomainEventModel = new ImagePathUpdateDomainEventModel();
    imagePathUpdateDomainEventModel.setStoreId(STORE_ID);
    imagePathUpdateDomainEventModel.setProductCode(PRODUCT_CODE);
    oldAndNewPathDomainEventModel1 = new OldAndNewPathDomainEventModel();
    oldAndNewPathDomainEventModel1.setOldPath(OLD_PATH1);
    oldAndNewPathDomainEventModel1.setNewPath(NEW_PATH1);
    oldAndNewPathDomainEventModel2 = new OldAndNewPathDomainEventModel();
    oldAndNewPathDomainEventModel2.setOldPath(OLD_PATH2);
    oldAndNewPathDomainEventModel2.setNewPath(NEW_PATH2);
    imagePathUpdateDomainEventModel.setImageUpdatedPath(
        new HashSet<>(Arrays.asList(oldAndNewPathDomainEventModel1, oldAndNewPathDomainEventModel2)));

    ReflectionTestUtils.setField(instance, "decrementVendorQuotaCounter", false);
    ReflectionTestUtils.setField(instance, "deleteOriginalImages", true);
    ReflectionTestUtils.setField(instance, "resizeImagePrefix", "resize,/resize");
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", true);
  }

  private void setPrimaryFilterResponse() {
    int index = 0;
    for (String filter : primaryFilterValues) {
      primaryFilterResponse[index++] = index;
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    MDC.remove(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    Mockito.verifyNoMoreInteractions(this.productPublisherService);
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productAttributeRespository);
    Mockito.verifyNoMoreInteractions(this.productImageRepository);
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(this.productItemRepository);
    Mockito.verifyNoMoreInteractions(this.productItemImageRepository);
    Mockito.verifyNoMoreInteractions(this.productItemAttributeRepository);
    Mockito.verifyNoMoreInteractions(this.taskHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(productImageQcFeedbackService);
    Mockito.verifyNoMoreInteractions(this.youTube);
    Mockito.verifyNoMoreInteractions(this.solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(this.approvedProductPublisherService);
    Mockito.verifyNoMoreInteractions(this.mapper);
    Mockito.verifyNoMoreInteractions(this.applicationContext);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.productServiceRepository);
    Mockito.verifyNoMoreInteractions(this.distributionTaskService);
    Mockito.verifyNoMoreInteractions(this.vendorQuotaCounterRepository);
    deleteFolder(IMAGE_SOURCE_DIRECTORY);
    Mockito.verifyNoMoreInteractions(this.solrReindexPublisherService);
    Mockito.verifyNoMoreInteractions(this.kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(this.appealProductService);
  }

  @Test public void testCreateProduct() {
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(null);
    Mockito.when(this.productRepository.saveAndFlush(this.product)).thenReturn(this.product);
    Product productResponse = this.instance.createProduct(this.product);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1)).saveAndFlush(this.product);
    Assertions.assertEquals(this.product, productResponse);
  }

  @Test
   void testGetEditedByMerchantNotEdited() {
    Mockito.when(this.productRepository.findByProductCodeAndVersionAndMarkForDeleteFalse(PRODUCT_CODE,VERSION))
        .thenReturn(new Product());
    Boolean edited = this.instance.getEditedByMerchant(PRODUCT_CODE, VERSION);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndVersionAndMarkForDeleteFalse(PRODUCT_CODE,VERSION);
    Assertions.assertFalse(edited);
  }

  @Test
   void testGetEditedByMerchantEdited() {
    Mockito.when(this.productRepository.findByProductCodeAndVersionAndMarkForDeleteFalse(PRODUCT_CODE,VERSION))
        .thenReturn(null);
    Boolean edited = this.instance.getEditedByMerchant(PRODUCT_CODE, VERSION);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndVersionAndMarkForDeleteFalse(PRODUCT_CODE,VERSION);
    Assertions.assertTrue(edited);
  }

  @Test
   void testCreateProductException() {
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
      .thenReturn(this.product);
    Product productResponse = null;
    Assertions.assertThrows(Exception.class, () -> this.instance.createProduct(this.product));
    Mockito.verify(this.productRepository, Mockito.times(1))
      .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(0)).saveAndFlush(this.product);
    Assertions.assertNull(productResponse);
  }

  @Test public void testGetAllProductDetailsByCode() throws Exception {
    ReflectionTestUtils.setField(instance,"autoSolrReindexingEnabled",true);
    Product product = getProduct();

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Product productResponse = this.instance.getAllProductDetailsByCode(Mockito.anyString());
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productUtils, Mockito.times(1))
        .initializeAllProductDetails(any());
    Assertions.assertEquals(product, productResponse);
    Assertions.assertEquals(product.getProductImages().get(0).getSequence(), Integer.valueOf(0));
    Assertions.assertEquals(
        product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),
        Integer.valueOf(0));
  }

  @Test
   void testGetAllProductDetailsByNullProduct() throws Exception {
    ReflectionTestUtils.setField(instance, "autoSolrReindexingEnabled", true);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getAllProductDetailsByCode(PRODUCT_CODE));
    }finally {
      Mockito.verify(this.productRepository, Mockito.times(1)).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
      Mockito.verify(this.productRepository, Mockito.times(1)).findByProductCode(PRODUCT_CODE);
      Mockito.verify(solrReindexPublisherService).publishPDTProductSolrBatchDeleteDomainEventModelForReindex(any(PDTProductSolrDeleteDomainEventModel.class));
    }
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOn() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Product productResponse = this.instance.getAllProductDetailsByCode(Mockito.anyString());
    Mockito.verify(this.productRepository).findByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productUtils).initializeAllProductDetails(any());
    Assertions.assertEquals(product, productResponse);
    Assertions.assertEquals(product.getProductImages().get(0).getSequence(), Integer.valueOf(0));
    Assertions.assertEquals(
        product.getProductItems().get(0).getProductItemImages().get(0).getSequence(),
        Integer.valueOf(0));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImagesEmpty() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductImages(new ArrayList<>());
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setProductImages(null);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getImages().forEach(image -> image.setLocationPath(LOCATION_PATH+
      "/resize"));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

  }

  @Test
   void autoHealProductDataTest() throws Exception {
    instance.autoHealProductData(product, Constants.AUTOHEAL);
  }

  @Test
   void autoHealProductDataGoodDataTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
  }

  @Test
   void autoHealProductDataGoodDataMfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setMarkForDelete(true);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(product.getProductCode(), true, true))
        .thenReturn(productDetailResponse);
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(product.getProductCode(), true, true);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(product.getProductCode()),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataGoodDataItemMfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setMarkForDelete(true);
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(product.getProductCode(), true, true))
        .thenReturn(productDetailResponse);
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(product.getProductCode(), true, true);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(product.getProductCode()),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataGoodDataTest2() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
       product.getProductImages().forEach(productImage -> productImage.setMarkForDelete(true));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void autoHealProductDataOnlyItemAttributesEmptyTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    product.getProductItems().iterator().next().setProductItemAttributes(new ArrayList<>());
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataOnlyAllItemAttributesMfdTrueEmptyTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setMarkForDelete(true);
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    product.getProductItems().iterator().next().setProductItemAttributes(new ArrayList<>());
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());


  }

  @Test
   void autoHealProductDataGoodDataTestWithItemImageMFDTrue() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.setProductCode(PRODUCT_CODE);
    product.getProductItems().forEach(productItem -> productItem.getProductItemImages()
      .forEach(productItemImage -> productItemImage.setMarkForDelete(true)));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void autoHealProductDataGoodDataTestWithAttributesMFDTrue() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setMarkForDelete(true);
    productAttribute.setProduct(product);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

  }



  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductAttributesEmpty() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    product.setProductAttributes(new ArrayList<>());
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    Mockito.when(productUtils.productContainsEmptyItemAttributes(Mockito.any())).thenReturn(true);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_mfdTrue2() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    Product product = getProduct();
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(true);
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("INACTIVE"), Collections.emptyMap()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH);
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Collections.singletonList(image2));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    product.getProductItems().forEach(productItem -> productItem.setMarkForDelete(true));
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setPostLive(true);
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(false);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_mfdTrueEdited() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    product.setEdited(true);
    product.setPostLive(false);
    product.setRevised(false);
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH);
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Collections.singletonList(image2));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    product.getProductItems().forEach(productItem -> productItem.setMarkForDelete(true));
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("IN-ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_mfdTrueEditedRevised() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    product.setEdited(false);
    product.setPostLive(false);
    product.setRevised(true);
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH);
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Collections.singletonList(image2));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(false);
    product.getProductItems().forEach(productItem -> productItem.setMarkForDelete(true));
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }


  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_mfdTrueRevised() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.setEdited(false);
    product.setRevised(false);
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH);
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Collections.singletonList(image2));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    product.getProductItems().forEach(productItem -> productItem.setMarkForDelete(true));
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.setPostLive(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }



  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemsEmpty() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",false);

    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setProductItems(new ArrayList<>());
    product.setPostLive(true);
    product.setEdited(true);
    product.setRevised(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setPostLive(true);
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(true);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemsEmptyEdited() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",false);

    Product product = getProduct();
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(false);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setProductItems(new ArrayList<>());
    product.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLiveEdited() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    product.setProductItems(Collections.emptyList());
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(false);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLiveEdited2() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(false);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
   productDetailResponse.setPostLive(true);
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(false);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLive() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setPostLive(true);
    product.setEdited(true);
    product.setRevised(false);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(false);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);

    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLiveRevised() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLiveRevised2() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setPostLive(true);
    product.setEdited(true);
    product.setRevised(true);
    productAttribute.setProduct(product);

    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(false);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPReliveRevised() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    product.setProductCode(PRODUCT_CODE);
    productAttribute.setName(NOTES);
    product.setPostLive(false);
    product.setEdited(true);
    product.setRevised(false);
    productAttribute.setProduct(product);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(false);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPReliveRevisedSwitchOff() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",false);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setName(NOTES);
    product.setPostLive(false);
    product.setEdited(true);
    product.setRevised(true);
    productAttribute.setProduct(product);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(true);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnPostLiveEditedRevised() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setName(NOTES);
    product.setPostLive(false);
    product.setEdited(false);
    product.setRevised(false);
    productAttribute.setProduct(product);
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(CODE));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(new ArrayList<>()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(false);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setProduct(product);
    productAttribute1.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Product productResponse = this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }


  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImagesEmpty() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.getProductItems().get(0).setProductItemImages(null);
    product.setPostLive(false);
    product.setEdited(true);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImagesEditedScenario() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.getProductItems().get(0).setProductItemImages(null);
    product.setPostLive(true);
    product.setEdited(true);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImagesEmptySwitchOff() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", false);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.getProductItems().get(0).setProductItemImages(null);
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_mfdTrue() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(true);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH.concat("resize"));
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH);
    ProductItemImage image3 = new ProductItemImage();
    image3.setActive(true);
    image3.setMarkForDelete(true);
    image3.setOriginalImage(false);
    image3.setLocationPath(LOCATION_PATH.concat("resize"));
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(image2, image3));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(LOCATION_PATH.concat("resize")));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.getImages()
        .forEach(image1 -> image1.setLocationPath("resize")));
    productDetailResponse.getProductItemResponses().forEach(
      productItemResponse -> productItemResponse.getImages()
        .forEach(image1 -> image1.setMarkForDelete(true)));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_nullLocation() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(true);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH.concat("/resize"));
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(true);
    image2.setOriginalImage(false);
    image2.setLocationPath(LOCATION_PATH.concat("/catalog-image"));
    product.getProductItems().get(0).setProductItemImages(Collections.singletonList(image2));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_orphanedImages() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image.setEdited(true);
    ProductImage image10 = new ProductImage();
    image10.setActive(true);
    image10.setMarkForDelete(false);
    image10.setOriginalImage(false);
    image10.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    image10.setEdited(true);
    ProductImage image11 = new ProductImage();
    image11.setActive(true);
    image11.setMarkForDelete(false);
    image11.setOriginalImage(true);
    image11.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image11.setEdited(true);
    product.setProductImages(Arrays.asList(image,image10,image11));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(false);
    image2.setOriginalImage(true);
    image2.setEdited(true);
    image2.setLocationPath("/"+LOCATION_PATH+".jpeg");
    ProductItemImage image3 = new ProductItemImage();
    image3.setActive(true);
    image3.setMarkForDelete(false);
    image3.setOriginalImage(false);
    image3.setEdited(false);
    image3.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    ProductItemImage image4 = new ProductItemImage();
    image4.setActive(true);
    image4.setMarkForDelete(false);
    image4.setOriginalImage(true);
    image4.setEdited(true);
    image4.setLocationPath("/"+LOCATION_PATH+"2.jpeg");
    ProductItemImage image5 = new ProductItemImage();
    image5.setActive(true);
    image5.setMarkForDelete(false);
    image5.setOriginalImage(true);
    image5.setEdited(false);
    image5.setLocationPath("/"+LOCATION_PATH+"3.jpeg");
    ProductItemImage image6 = new ProductItemImage();
    image6.setActive(false);
    image6.setMarkForDelete(false);
    image6.setOriginalImage(true);
    image6.setEdited(true);
    image6.setLocationPath("/"+LOCATION_PATH+"4.jpeg");
    ProductItemImage image7 = new ProductItemImage();
    image7.setActive(false);
    image7.setMarkForDelete(false);
    image7.setOriginalImage(false);
    image7.setEdited(true);
    image7.setLocationPath("/"+LOCATION_PATH+"4.jpeg");
    product.getProductItems().get(0)
        .setProductItemImages(Arrays.asList(image2, image3, image4, image5, image6, image7));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImageBucket");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("sourceImageBucket");
    Mockito.when(gcsService.copyImage(anyString(), anyString(), anyString(), anyString())).thenReturn(true);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImages_orphanedImages() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image.setEdited(true);
    product.setProductImages(List.of(image));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImageBucket");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("sourceImageBucket");
    Mockito.when(gcsService.copyImage(anyString(), anyString(), anyString(), anyString())).thenReturn(true);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductAndItemImages_orphanedImages() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image.setEdited(true);
    product.setProductImages(List.of(image));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(false);
    image2.setOriginalImage(true);
    image2.setEdited(true);
    image2.setLocationPath("/"+LOCATION_PATH+".jpeg");
    ProductItemImage image3 = new ProductItemImage();
    image3.setActive(true);
    image3.setMarkForDelete(false);
    image3.setOriginalImage(false);
    image3.setEdited(false);
    image3.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    ProductItemImage image4 = new ProductItemImage();
    image4.setActive(true);
    image4.setMarkForDelete(false);
    image4.setOriginalImage(true);
    image4.setEdited(true);
    image4.setLocationPath("/"+LOCATION_PATH+"2.jpeg");
    ProductItemImage image5 = new ProductItemImage();
    image5.setActive(true);
    image5.setMarkForDelete(false);
    image5.setOriginalImage(true);
    image5.setEdited(false);
    image5.setLocationPath("/"+LOCATION_PATH+"3.jpeg");
    ProductItemImage image6 = new ProductItemImage();
    image6.setActive(false);
    image6.setMarkForDelete(false);
    image6.setOriginalImage(true);
    image6.setEdited(true);
    image6.setLocationPath("/"+LOCATION_PATH+"4.jpeg");
    ProductItemImage image7 = new ProductItemImage();
    image7.setActive(false);
    image7.setMarkForDelete(false);
    image7.setOriginalImage(false);
    image7.setEdited(true);
    image7.setLocationPath("/"+LOCATION_PATH+"4.jpeg");
    product.getProductItems().get(0)
        .setProductItemImages(Arrays.asList(image2, image3, image4, image5, image6, image7));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImageBucket");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("sourceImageBucket");
    Mockito.when(gcsService.copyImage(anyString(), anyString(), anyString(), anyString())).thenReturn(true);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }


  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImages_orphanedImagesWithNoOtherDetailEdited()
      throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image.setEdited(true);
    product.setProductImages(Collections.singletonList(image));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImages_noOrphanedImagesWithNoOtherDetailEdited()
      throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    product.getProductItems().iterator().next().setProductItemAttributes(
        List.of(productItemAttribute));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    instance.autoHealProductData(product, Constants.AUTOHEAL);
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_noOrphanedImages() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image.setEdited(true);
    ProductImage image10 = new ProductImage();
    image10.setActive(true);
    image10.setMarkForDelete(false);
    image10.setOriginalImage(false);
    image10.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    image10.setEdited(true);
    ProductImage image11 = new ProductImage();
    image11.setActive(true);
    image11.setMarkForDelete(false);
    image11.setOriginalImage(true);
    image11.setLocationPath("/"+LOCATION_PATH+".jpeg");
    image11.setEdited(true);
    product.setProductImages(Arrays.asList(image,image10,image11));
    ProductItemImage image2 = new ProductItemImage();
    image2.setActive(true);
    image2.setMarkForDelete(false);
    image2.setOriginalImage(true);
    image2.setEdited(true);
    image2.setLocationPath("/"+LOCATION_PATH+".jpeg");
    ProductItemImage image3 = new ProductItemImage();
    image3.setActive(true);
    image3.setMarkForDelete(false);
    image3.setOriginalImage(false);
    image3.setEdited(true);
    image3.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(image2,image3));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductItemImages_orphanedImagesException() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "orphanedImageAutoHealFlag", true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    ProductImage image = new ProductImage();
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(true);
    image.setLocationPath(LOCATION_PATH);
    image.setEdited(true);
    product.setProductImages(Collections.singletonList(image));
    ProductItemImage image3 = new ProductItemImage();
    image3.setActive(true);
    image3.setMarkForDelete(false);
    image3.setOriginalImage(false);
    image3.setEdited(true);
    image3.setLocationPath("resize/"+LOCATION_PATH+".jpeg");
    ProductItemImage image4 = new ProductItemImage();
    image4.setActive(true);
    image4.setMarkForDelete(false);
    image4.setOriginalImage(true);
    image4.setEdited(true);
    image4.setLocationPath("/"+LOCATION_PATH+"2.jpeg");
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(image3,image4));
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(true);
    productAttribute.setProduct(product);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getImages().forEach(image1 -> image1.setLocationPath(null));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
        .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
            Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImageBucket");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("sourceImageBucket");
    Mockito.when(gcsService.copyImage(anyString(), anyString(), anyString(), anyString())).thenReturn(true);
    Product responseProduct = getProduct();
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
        true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }




  private ProductDetailResponse getProductDetailResponse() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CODE);
    productCategoryResponse.setCategory(categoryResponse);
    productDetailResponse.setProductCategoryResponses(Collections.singletonList(productCategoryResponse));
    productDetailResponse.setCreatedBy(Constants.DEFAULT_USERNAME);
    Image image = new Image();
    image.setMainImages(true);
    image.setActive(true);
    image.setMarkForDelete(false);
    image.setOriginalImage(false);
    image.setLocationPath(LOCATION_PATH);
    productDetailResponse.setImages(Collections.singletonList(image));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(CODE);
    productItemResponse.setImages(Collections.singletonList(image));
    productDetailResponse.setProductItemResponses(Collections.singleton(productItemResponse));
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(VALUE);
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(CODE);
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productAttributeResponse.setAttribute(attributeResponse);
    productDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    productDetailResponse.setCreatedBy(USERNAME);
    return productDetailResponse;
  }

  private Product getProduct() {
    Product product = new Product.Builder().productName("productName").productCode("code").edited(false)
        .productAttributes(new ArrayList<>())
        .productImages(new ArrayList<>()).productItems(new ArrayList<>())
        .build();
    product.getProductImages().add(new ProductImage(product, "location", 1, false));
    product.getProductImages().add(new ProductImage(product, "location", 0, true));
    product.getProductItems().add(new ProductItem());
    product.getProductItems().get(0).setProductItemImages(new ArrayList());
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setEdited(true);
    image1.setSequence(0);
    product.getProductImages().get(0).setEdited(false);
    product.getProductItems().get(0).getProductItemImages().add(image);
    product.getProductItems().get(0).getProductItemImages().add(image1);
    return product;
  }

  @Test
   void testGetAllProductDetailsByCodeNull() throws Exception {
    Product product =
        new Product.Builder().productName("productName").productCode("code").productAttributes(new ArrayList<>())
            .productImages(new ArrayList<>()).productItems(new ArrayList<>()).build();

    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.getAllProductDetailsByCode(Mockito.anyString()));
    Mockito.verify(this.productRepository, Mockito.times(1))
      .findByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findByProductCode(anyString());
    Mockito.verify(this.productUtils, Mockito.times(0)).initializeAllProductDetails(any());
  }

  @Test
   void testGetAllProductDetailsByCodeNull_2() throws Exception {
    Product product =
        new Product.Builder().productName("productName").productCode("code").productAttributes(new ArrayList<>())
            .productImages(new ArrayList<>()).productItems(new ArrayList<>()).build();
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.any())).thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.getAllProductDetailsByCode(null));
      Mockito.verify(this.productRepository, Mockito.times(1))
          .findByProductCodeAndMarkForDeleteFalse(Mockito.any());
      Mockito.verify(this.productUtils, Mockito.times(0)).initializeAllProductDetails(any());
  }

  @Test
   void testGetAllProductDetailsByCodeNullException() throws Exception {
    Product product = new Product.Builder().productName("productName").state(WorkflowState.PASSED)
      .productCode("code").productAttributes(new ArrayList<>()).productImages(new ArrayList<>())
      .productItems(new ArrayList<>()).build();

    Mockito.when(this.productRepository.findByProductCode(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.getAllProductDetailsByCode(Mockito.anyString()));
    Mockito.verify(this.productRepository, Mockito.times(1))
      .findByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findByProductCode(anyString());
    Mockito.verify(this.productUtils, Mockito.times(0)).initializeAllProductDetails(any());
  }

  @Test
   void testGetProductByCode() {
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(this.product);
    Product productResponse = this.instance.getProductByCode(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(this.product, productResponse);
  }

  @Test
   void testGetProductByProductCodeAndMarkForDelete() {
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(this.product);
    Product productResponse = this.instance.getProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Assertions.assertEquals(this.product, productResponse);
    Assertions.assertEquals(this.product.isMarkForDelete(), productResponse.isMarkForDelete());
  }

  @Test
   void getDetailsForProductByProductCodeAndMarkForDeleteFalseTest() {
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(this.product);
    Product productResponse = this.instance.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Assertions.assertEquals(this.product, productResponse);
    Assertions.assertEquals(this.product.isMarkForDelete(), productResponse.isMarkForDelete());
  }

  @Test
   void getDetailsForProductByProductCodeTest() {
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
        .thenReturn(this.product);
    Product productResponse = this.instance.getDetailsForProductByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Assertions.assertEquals(this.product, productResponse);
    Assertions.assertEquals(this.product.isMarkForDelete(), productResponse.isMarkForDelete());
  }

  @Test
   void getDetailsForProductByProductCodeNullTest() {
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
        .thenReturn(null);
    this.instance.getDetailsForProductByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(PRODUCT_CODE);
  }

  @Test
   void getDetailsForProductByProductCodeAndMarkForDeleteFalseNullTest() {
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(null);
    Product productResponse = this.instance.getDetailsForProductByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Assertions.assertNull(productResponse);
  }

  @Test public void testGetProductListByCodes() {
    List<String> productList = new ArrayList<>(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE2));
    Mockito.when(this.productRepository.findByProductCode(Mockito.anyList(), Mockito.anyList()))
        .thenReturn(createProductList());
    this.instance.getProductListByProductCodes(productList, new ArrayList<WorkflowState>(
        Arrays.asList(WorkflowState.UNASSIGNED, WorkflowState.IN_REVIEW)));
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(Mockito.anyList(), Mockito.anyList());
  }

  @Test public void testRejectProduct() throws Exception {
    Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID))
        .thenReturn(this.product);
    Mockito.doNothing().when(this.productRepository)
        .updateWorkflowState(PRODUCT_ID, WorkflowState.REJECTED);
    Mockito.when(this.productPublisherService.rejectedProductPublisher(PRODUCT_CODE))
        .thenReturn(this.productRejectedEventModel);
    this.instance.rejectAndDiscardProduct(PRODUCT_ID, false);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .updateWorkflowState(PRODUCT_ID, WorkflowState.REJECTED);
    Mockito.verify(this.productBusinessPartnerService, Mockito.times(1))
        .republishToPDT(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test public void testRejectProductSLA() throws Exception {
    Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID))
        .thenReturn(this.product);
    Mockito.doNothing().when(this.productRepository)
        .updateWorkflowState(PRODUCT_ID, WorkflowState.EXCEEDED_SLA);
    Mockito.when(this.productPublisherService.rejectedProductPublisher(PRODUCT_CODE))
        .thenReturn(this.productRejectedEventModel);
    this.instance.rejectAndDiscardProduct(PRODUCT_ID, true);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .updateWorkflowState(PRODUCT_ID, WorkflowState.EXCEEDED_SLA);
    Mockito.verify(this.productBusinessPartnerService, Mockito.times(1))
        .republishToPDT(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void rejectAndDiscardProductTest_WhenProductNotFound() throws Exception {
    try {
      Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID)).thenReturn(null);
      Mockito.doNothing().when(this.productRepository).updateWorkflowState(PRODUCT_ID, WorkflowState.EXCEEDED_SLA);
      Mockito.when(this.productPublisherService.rejectedProductPublisher(PRODUCT_CODE)).thenReturn(this
          .productRejectedEventModel);
      this.instance.rejectAndDiscardProduct(PRODUCT_ID, true);
    } catch(IllegalArgumentException ex) {
      Mockito.verify(this.productRepository).findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    }
  }

  @Test
   void testRejectProductException() throws Exception {
    this.product.setState(WorkflowState.REJECTED);
    Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID))
      .thenReturn(this.product);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.rejectAndDiscardProduct(PRODUCT_ID, true));
    Mockito.verify(this.productRepository, Mockito.times(1))
      .findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(0))
      .updateWorkflowState(PRODUCT_ID, WorkflowState.REJECTED);
    Mockito.verify(this.productBusinessPartnerService, Mockito.times(0))
      .republishToPDT(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test public void testReplaceProduct() {
    this.product.setState(WorkflowState.REJECTED);
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
        .thenReturn(this.oldProduct);
    this.instance.replaceProduct(this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productItemRepository, Mockito.times(1))
        .deleteAll(this.oldProduct.getProductItems());
    Mockito.verify(this.productAttributeRespository, Mockito.times(1))
        .deleteAll(this.oldProduct.getProductAttributes());
    Mockito.verify(this.productImageRepository, Mockito.times(1))
        .deleteAll(this.oldProduct.getProductImages());
    Mockito.verify(this.productUtils, Mockito.times(1))
        .regenerateProductReplacementDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).saveAndFlush(this.oldProduct);
  }

  @Test public void testReplaceProductDetailsNull() {
    this.product.setState(WorkflowState.REJECTED);
    this.oldProduct.getProductImages().clear();
    this.oldProduct.getProductItems().clear();
    this.oldProduct.getProductAttributes().clear();
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
        .thenReturn(this.oldProduct);
    this.instance.replaceProduct(this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productItemRepository, Mockito.times(0))
        .deleteAll(this.oldProduct.getProductItems());
    Mockito.verify(this.productAttributeRespository, Mockito.times(0))
        .deleteAll(this.oldProduct.getProductAttributes());
    Mockito.verify(this.productImageRepository, Mockito.times(0))
        .deleteAll(this.oldProduct.getProductImages());
    Mockito.verify(this.productUtils, Mockito.times(1))
        .regenerateProductReplacementDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).saveAndFlush(this.oldProduct);
  }

  @Test
   void testReplaceProductException() {
    this.oldProduct.setState(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
        .thenReturn(this.oldProduct);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.replaceProduct(this.newProduct));
      Mockito.verify(this.productRepository, Mockito.times(1))
          .findByProductCode(PRODUCT_CODE);
      Mockito.verify(this.productItemRepository, Mockito.times(0))
          .deleteAll(this.oldProduct.getProductItems());
      Mockito.verify(this.productAttributeRespository, Mockito.times(0))
          .deleteAll(this.oldProduct.getProductAttributes());
      Mockito.verify(this.productImageRepository, Mockito.times(0))
          .deleteAll(this.oldProduct.getProductImages());
      Mockito.verify(this.productUtils, Mockito.times(0))
          .regenerateProductReplacementDetails(this.oldProduct, this.newProduct);
      Mockito.verify(this.productRepository, Mockito.times(0)).saveAndFlush(this.oldProduct);
  }


  @Test
   void testReplaceProductException2() {
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    Assertions.assertThrows(Exception.class, () -> this.instance.replaceProduct(this.newProduct));
    Mockito.verify(this.productRepository, Mockito.times(1)).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productItemRepository, Mockito.times(0))
      .deleteAll(this.oldProduct.getProductItems());
    Mockito.verify(this.productAttributeRespository, Mockito.times(0))
      .deleteAll(this.oldProduct.getProductAttributes());
    Mockito.verify(this.productImageRepository, Mockito.times(0))
      .deleteAll(this.oldProduct.getProductImages());
    Mockito.verify(this.productUtils, Mockito.times(0))
      .regenerateProductReplacementDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(0)).saveAndFlush(this.oldProduct);
  }

  @Test public void testUpdateProduct() throws Exception {
    this.instance.updateProduct(this.product);
    Mockito.verify(this.productRepository, Mockito.times(1)).save(this.product);
  }

  @Test public void testUpdateProductNull() throws Exception {
    this.instance.updateProduct(null);
    Mockito.verify(this.productRepository, Mockito.times(0)).save(this.product);
  }

  @Test public void testUpdateProductException() throws Exception {
    Mockito.when(this.productRepository.save(this.product)).thenThrow(RuntimeException.class);
    this.instance.updateProduct(this.product);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());

  }

  @Test
   void testUpdateProductDetails() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateProductDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils, Mockito.times(1))
        .replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.productUtils).getProductDetailChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
  }

  @Test
   void testUpdateProductDetailsBrandNull() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setBrand(BRAND_NAME);
    newProduct.setBrandCode(null);
    Mockito.when(pbpFeign
            .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateProductDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils, Mockito.times(1))
        .replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.productUtils).getProductDetailChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(this.productUtils).getBrandCodeByBrandName(BRAND_NAME);
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
  }

  @Test
   void testUpdateProductDetails_WithSuccessFalse() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    productSystemParameterResponse.setValue("false");
    GdnRestSingleResponse gdnRestSingleResponse =
      new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(), Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(gdnRestSingleResponse);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.updateProductDetails(this.oldProduct, this.newProduct));
    Mockito.verify(pbpFeign).findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(),
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      GdnMandatoryRequestParameterUtil.getRequestId(), Constants.YOUTUBE_URL_VALIDATION_SWITCH);
  }

  @Test
   void testUpdateProductDetailsWithValidUrl() throws Exception {
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(anyString(), anyString(), any(), any(), anyString(), any(WorkflowState.class),
            anyString())).thenReturn(new PDTHistoryEventModel());
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_URL);
    Mockito.when(this.productUtils.validateYouTubeUrl(YOUTUBE_URL, youTube)).thenReturn(true);
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateProductDetails(this.oldProduct, this.newProduct);
    Mockito.verify(youTube).videos();
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.productUtils).getProductDetailChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.any(), Mockito.any());
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT), anyString(), any());
  }

  @Test
   void testUpdateProductDetailsExceptionSystemParamaterTest() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_URL);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, productSystemParameterResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateProductDetails(this.oldProduct, this.newProduct));
    } finally {
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    }
  }

  @Test
   void testUpdateProductDetailsWithInvalidUrlWithSwitchOff() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(INVALID_URL);
    productSystemParameterResponse.setValue("false");
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateProductDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.productUtils).getProductDetailChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void testUpdateProductDetailsWithInvalidUrl() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setVideoUrl(YOUTUBE_URL);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_INVALID_URL);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateProductDetails(this.oldProduct, this.newProduct));
    } finally {
      Mockito.verify(this.productUtils).validateYouTubeUrl(YOUTUBE_INVALID_URL, youTube);
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    }
  }

  @Test
   void testUpdateProductDetailsProtectedBrand() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setVideoUrl(YOUTUBE_URL);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    Mockito.when(pbpFeign
            .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(productUtils.validateProtectedBrand(Mockito.anyString(), Mockito.any())).thenReturn(false);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateProductDetails(this.oldProduct, this.newProduct));
    } finally {
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    }
  }

  @Test
   void testUpdateProductDetailsWithoutAnyChange() throws Exception {
    newProduct.setVideoUrl(VALUE);
    productHistoryDTOS = new ArrayList<>();
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(productUtils.getProductDetailChanges(this.oldProduct, this.newProduct)).thenReturn(productHistoryDTOS);
    Mockito.when(productUtils.replaceProductDetails(this.oldProduct, this.newProduct, true)).thenReturn(this.oldProduct);
    this.instance.updateProductDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.productUtils).getProductDetailChanges(this.oldProduct, this.newProduct);
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
  }

  @Test
   void testUpdateEditedProductDetails() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct, modifiedFields);
    Mockito.verify(this.productUtils, Mockito.times(1))
        .replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void testUpdateEditedProductUpdateProductImageDetails() throws Exception {
    ReflectionTestUtils.setField(instance, "refreshProductImageDetails", true);
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(pbpFeign
            .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct, modifiedFields);
    Mockito.verify(this.productUtils, Mockito.times(1))
        .replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void testUpdateEditedProductDetails_WithSuccessFalse() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    productSystemParameterResponse.setValue("false");
    GdnRestSingleResponse gdnRestSingleResponse =
        new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct,
          modifiedFields));
    } finally {
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    }
  }

  @Test
   void testUpdateEditedProductDetailsWithValidUrl() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_URL);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(this.productUtils.validateYouTubeUrl(YOUTUBE_URL, youTube)).thenReturn(true);
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct, modifiedFields);
    Mockito.verify(youTube).videos();
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void testUpdateEditedProductDetailsExceptionSystemParameterTest() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_URL);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, productSystemParameterResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct,
          modifiedFields));
    } finally {
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    }
  }

  @Test
   void testUpdateEditedProductDetailsWithInvalidUrlWithSwitchOff() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(INVALID_URL);
    productSystemParameterResponse.setValue("false");
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct, null);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Assertions.assertEquals(IMAGE_VIOLATION, productCaptor.getValue().getImageViolations());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
  }

  @Test
   void testUpdateEditedProductDetailsWithInvalidUrl() throws Exception {
    oldProduct.setProductPredictionScore(11);
    oldProduct.setVideoUrl(YOUTUBE_URL);
    oldProduct.setImageViolations(IMAGE_VIOLATION);
    newProduct.setVideoUrl(YOUTUBE_INVALID_URL);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct,
          modifiedFields));
    } finally {
      Mockito.verify(this.productUtils).validateYouTubeUrl(YOUTUBE_INVALID_URL, youTube);
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    }
  }

  @Test
   void testUpdateEditedProductDetailsWithoutAnyChange() throws Exception {
    newProduct.setVideoUrl(VALUE);
    productHistoryDTOS = new ArrayList<>();
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(productUtils.replaceProductDetails(this.oldProduct, this.newProduct, true)).thenReturn(this.oldProduct);
    this.instance.updateEditedProductDetails(this.oldProduct, this.newProduct, modifiedFields);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductDetails(this.oldProduct, this.newProduct, true);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
  }

  @Test public void testRejectProduct1() throws Exception {
    List<ProductItem> productItems = this.newProduct.getProductItems();
    for (ProductItem productItem : productItems) {
      ProductItemImage productItemImage = new ProductItemImage("test", productItem, true, 0);
      ProductItemAttribute productItemAttribute =
          new ProductItemAttribute("10001", "attr-code", "attr-name", null, null);
      productItem.setProductItemImages(new ArrayList<ProductItemImage>());
      productItem.setProductItemAttributes(new ArrayList<ProductItemAttribute>());
      productItem.getProductItemImages().add(productItemImage);
      productItem.getProductItemAttributes().add(productItemAttribute);
    }
    this.newProduct.setRevised(true);
    this.instance.removeProductWithMarkForDelete(this.newProduct, "username");
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, "username");
    Assertions.assertFalse(productArgumentCaptor.getValue().isRevised());
  }

  @Test public void testRejectProductProductItemNull() throws Exception {
    this.newProduct.setProductItems(null);
    this.instance.removeProductWithMarkForDelete(this.newProduct, StringUtils.EMPTY);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
  }

  @Test public void testRejectProductProductItemDetailsNull() throws Exception {
    List<ProductItem> productItems = this.newProduct.getProductItems();
    for (ProductItem productItem : productItems) {
      productItem.setProductItemImages(null);
      productItem.setProductItemAttributes(null);
    }

    this.newProduct.setProductImages(null);
    this.newProduct.setProductAttributes(null);

    this.instance.removeProductWithMarkForDelete(this.newProduct, StringUtils.EMPTY);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
  }

  @Test
   void testRejectProduct1NullException() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.productRepository).save(any());
    Assertions.assertThrows(RuntimeException.class,
      () -> this.instance.removeProductWithMarkForDelete(new Product(), StringUtils.EMPTY));
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
  }

  @Test
   void testRejectProduct1Exception() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> this.instance.removeProductWithMarkForDelete(null, StringUtils.EMPTY));
  }

  @Test public void testGetWorkflowStatusForProducts1() throws Exception {
    List<ProductDistributionTask> productDistributionTaskList =
        new ArrayList<ProductDistributionTask>();
    this.product.setId(PRODUCT_ID);
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    List<String> productList = new ArrayList<String>(Arrays.asList(PRODUCT_ID, PRODUCT_ID2));
    Mockito.when(this.productDistributionTaskRepository.getStatusForProducts(productList))
        .thenReturn(productDistributionTaskList);
    this.instance.getWorkflowStatusForProducts(productList);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getStatusForProducts(productList);
  }

  @Test public void testGetWorkflowStatusForProducts2() throws Exception {
    List<ProductDistributionTask> productDistributionTaskList =
        new ArrayList<ProductDistributionTask>();
    Vendor vendor = new Vendor();
    vendor.setId(VENDOR_ID);
    this.product.setId(PRODUCT_ID);
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    productDistributionTask =
        new ProductDistributionTask(TASK_CODE, vendor, this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    List<String> productList = new ArrayList<String>(Arrays.asList(PRODUCT_ID, PRODUCT_ID2));
    Mockito
        .when(this.productDistributionTaskRepository.getStatusForProducts(VENDOR_ID, productList))
        .thenReturn(productDistributionTaskList);
    this.instance.getWorkflowStatusForProducts(VENDOR_ID, productList);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getStatusForProducts(VENDOR_ID, productList);
  }

  @Test public void testGetWorkflowStatusForProducts2ProductListNull() throws Exception {
    List<ProductDistributionTask> productDistributionTaskList =
        new ArrayList<ProductDistributionTask>();
    Vendor vendor = new Vendor();
    vendor.setId(VENDOR_ID);
    this.product.setId(PRODUCT_ID);
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    productDistributionTask =
        new ProductDistributionTask(TASK_CODE, vendor, this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    List<String> productList = new ArrayList<String>(Arrays.asList(PRODUCT_ID, PRODUCT_ID2));
    Mockito
        .when(this.productDistributionTaskRepository.getStatusForProducts(VENDOR_ID, productList))
        .thenReturn(productDistributionTaskList);
    this.instance.getWorkflowStatusForProducts(VENDOR_ID, null);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
        .getStatusForProducts(VENDOR_ID, productList);
  }

  @Test public void testGetWorkflowStatusForProducts2VendorNull() throws Exception {
    List<ProductDistributionTask> productDistributionTaskList =
        new ArrayList<ProductDistributionTask>();
    Vendor vendor = new Vendor();
    vendor.setId(VENDOR_ID);
    this.product.setId(PRODUCT_ID);
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    productDistributionTask =
        new ProductDistributionTask(TASK_CODE, vendor, this.product, WorkflowState.IN_REVIEW,
            new Date());
    productDistributionTaskList.add(productDistributionTask);
    List<String> productList = new ArrayList<String>(Arrays.asList(PRODUCT_ID, PRODUCT_ID2));
    Mockito
        .when(this.productDistributionTaskRepository.getStatusForProducts(VENDOR_ID, productList))
        .thenReturn(productDistributionTaskList);
    this.instance.getWorkflowStatusForProducts(null, productList);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
        .getStatusForProducts(VENDOR_ID, productList);
  }


  @Test public void findByProductId() throws Exception {
    Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID))
        .thenReturn(this.product);
    this.instance.findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(this.productUtils, Mockito.times(1)).initializeAllProductDetails(this.product);
  }

  @Test
   void findByProductIdNull() throws Exception {
    Mockito.when(this.productRepository.findByIdAndMarkForDeleteFalse(PRODUCT_ID)).thenReturn(null);
    Assertions.assertThrows(Exception.class, () -> this.instance.findByProductId(PRODUCT_ID));
    Mockito.verify(this.productRepository, Mockito.times(1))
      .findByIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(this.productUtils, Mockito.times(0)).initializeAllProductDetails(null);
  }

  @Test
   void testUpdateProductImageDetails() throws Exception {
    mockImageFile();
    this.instance.updateProductImageDetails(this.oldProduct, this.newProduct, Boolean.FALSE);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productUtils).getImageChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(UPDATE_HISTORY_NOTES, taskHistory.getReason());
  }

  @Test
   void testUpdateProductImageDetailsWithEventSwitchTrue() throws Exception {
    mockImageFile();
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(any(), any(), any(), any(), any(), any(WorkflowState.class), any()))
        .thenReturn(new PDTHistoryEventModel());
    this.instance.updateProductImageDetails(this.oldProduct, this.newProduct, Boolean.FALSE);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productUtils).getImageChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT), anyString(), any(PDTHistoryEventModel.class));
  }

  @Test
   void testUpdateProductImageDetailsEditedTrue() throws Exception {
    mockImageFile();
    this.oldProduct.setEdited(Boolean.TRUE);
    this.instance.updateProductImageDetails(this.oldProduct, this.newProduct, Boolean.FALSE);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productUtils).getImageChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(UPDATE_HISTORY_NOTES, taskHistory.getReason());
  }

  @Test
   void testUpdateProductImageDetailsDeleteOrginalImageTrueEditedTrue() throws Exception {
    mockImageFile();
    this.oldProduct.setEdited(Boolean.TRUE);
    this.instance.updateProductImageDetails(this.oldProduct, this.newProduct, Boolean.TRUE);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productUtils).getImageChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(UPDATE_HISTORY_NOTES, taskHistory.getReason());
  }

  @Test
   void testUpdateProductImageDetailsDeleteOrginalImageTrue() throws Exception {
    mockImageFile();
    this.oldProduct.setEdited(Boolean.FALSE);
    this.instance.updateProductImageDetails(this.oldProduct, this.newProduct, Boolean.TRUE);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
    Mockito.verify(this.productUtils).getImageChanges(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils).toJson(productHistoryDTOS);
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(productImageRepository).findByProductId(Mockito.anyString());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.anyString());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(UPDATE_HISTORY_NOTES, taskHistory.getReason());
  }

  @Test
   void testUpdateEditedProductImageDetails() throws Exception {
    mockImageFile();
    this.instance.updateEditedProductImageDetails(this.oldProduct, this.newProduct);
    Mockito.verify(this.productUtils, Mockito.times(1)).replaceProductImageAndProductItemImages(this.oldProduct, this.newProduct);
    Mockito.verify(this.productRepository, Mockito.times(1)).save((Product) any());
  }


  private void mockImageFile() throws IOException {
      mockFile(IMAGE_SOURCE_DIRECTORY + ROOT + IMAGE_LOCATION);
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
    width = 640;
    height = 320;
    img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test public void testGetVendorIdByVendorCode() throws Exception {
    this.instance.getvendorIdByVendorCode(VENDOR_CODE);
    Mockito.verify(this.vendorRepository, Mockito.times(1)).getVendorIdByVendorCode(VENDOR_CODE);
  }

  @Test public void updateStateTest() {
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.product));
    this.instance.updateState(this.product, WorkflowState.PASSED);
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any());
  }

  @Test
   void updateStateTest_Exception() {
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.ofNullable(null));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateState(this.product, WorkflowState.PASSED));
    } finally {
      Mockito.verify(this.productRepository).findById(Mockito.anyString());
    }
  }

  @Test
   void updateStateTestNull_Exception() {
    this.product.setId(null);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.ofNullable(null));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateState(this.product, WorkflowState.PASSED));
    } finally {
      Mockito.verify(this.productRepository, Mockito.times(0)).findById(Mockito.anyString());
    }
  }

  @Test
   void updateStateAndRemoveAssigneeDetailsTest() {
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.product));
    this.instance.updateStateAndRemoveAssigneeDetails(this.product, WorkflowState.PASSED);
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any());
  }

  @Test
   void updateStateAndRemoveAssigneeDetailsTest_Exception() {
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.ofNullable(null));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateStateAndRemoveAssigneeDetails(this.product,
          WorkflowState.PASSED));
    } finally {
      Mockito.verify(this.productRepository).findById(Mockito.anyString());
    }
  }

  @Test
   void autoHealProductDistributionTaskOffTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductDistributionTask", false);
    instance.autoHealProductDistributionTask(product);
  }

  @Test
   void autoHealProductDistributionTaskTaskNotEmptyTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductDistributionTask", true);
    Mockito.when(productDistributionTaskService
        .findStoreIdAndProductIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, product.getId()))
        .thenReturn(Collections.singletonList(productDistributionTask));
    instance.autoHealProductDistributionTask(product);
    Mockito.verify(productDistributionTaskService)
        .findStoreIdAndProductIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, product.getId());
  }

  @Test
   void autoHealProductDistributionTaskTaskEmptyTest() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductDistributionTask", true);
    instance.autoHealProductDistributionTask(product);
    Mockito.verify(productDistributionTaskService)
        .findStoreIdAndProductIdAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, product.getId());
    Mockito.verify(distributionTaskService)
        .generateDistributionTaskForProduct(Constants.DEFAULT_STORE_ID, product.getCurrentVendor(),
            Collections.singletonList(product), product.getState());
    Mockito.verify(productDistributionTaskService).saveProductDistributionTaskList(new ArrayList<>());
    verify(kafkaProducer).send(eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY),anyString(),
        any(ProductDataAutoFixHistoryListRequest.class));
  }

  @Test
   void updateStateAndRemoveAssigneeDetailsNull_Exception() {
    this.product.setId(null);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.ofNullable(null));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateStateAndRemoveAssigneeDetails(this.product,
          WorkflowState.PASSED));
    } finally {
      Mockito.verify(this.productRepository, Mockito.times(0)).findById(Mockito.anyString());
    }
  }

  @Test
   void approveProductByVendorProductNull() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> this.instance.approveProductByVendor(null, VENDOR_CODE, null, isQuickApproval, productReviewer,
      false));
  }

  @Test
   void approveProductByVendorNotTaskFoundTestOk() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> this.instance.approveProductByVendor(createProductList().get(0), VENDOR_CODE, null, isQuickApproval,
        productReviewer, false));
  }

  @Test
   void productAndItemImagePathUpdateTest() {
    Mockito.when(productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE))
        .thenReturn(PRODUCT_ID);
    Mockito.when(productItemRepository.getIdsByProductIdAndMarkForDeleteFalse(PRODUCT_ID)).thenReturn(PRODUCT_ITEM_IDS);
    this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel);
    Mockito.verify(productRepository).getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemRepository).getIdsByProductIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(productImageRepository, Mockito.times(2)).updateLocationPathByProductId(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    Mockito.verify(productItemImageRepository, Mockito.times(2)).updateLocationPathByProductItem(Mockito.anyString(), Mockito.anyList(),
        Mockito.anyString());
  }

  @Test
   void productAndItemImagePathUpdateProductIdEmptyTest() {
    Mockito.when(productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE))
        .thenReturn(StringUtils.EMPTY);
    this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel);
    Mockito.verify(productRepository).getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void productAndItemImagePathUpdateProductItemIdEmptyTest() {
    Mockito.when(productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE))
        .thenReturn(PRODUCT_ID);
    Mockito.when(productItemRepository.getIdsByProductIdAndMarkForDeleteFalse(PRODUCT_ID))
        .thenReturn(new ArrayList<>());
    this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel);
    Mockito.verify(productRepository).getIdByStoreIdAndProductCodeAndMarkForDelete(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productItemRepository).getIdsByProductIdAndMarkForDeleteFalse(PRODUCT_ID);
    Mockito.verify(productImageRepository, Mockito.times(2))
        .updateLocationPathByProductId(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void productAndItemImagePathUpdateOldPathEmptyTest() {
    Mockito.when(
            productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PRODUCT_ID);
    Mockito.when(productItemRepository.getIdsByProductIdAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(PRODUCT_ITEM_IDS);
    oldAndNewPathDomainEventModel1.setOldPath(StringUtils.EMPTY);
    imagePathUpdateDomainEventModel.setImageUpdatedPath(new HashSet<>(
        Collections.singletonList(oldAndNewPathDomainEventModel1)));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel));
    } finally {
      Mockito.verify(productRepository)
          .getIdByStoreIdAndProductCodeAndMarkForDelete(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(productItemRepository).getIdsByProductIdAndMarkForDeleteFalse(Mockito.anyString());
    }
  }

  @Test
   void productAndItemImagePathUpdateNewPathEmptyTest() {
    Mockito.when(
            productRepository.getIdByStoreIdAndProductCodeAndMarkForDelete(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PRODUCT_ID);
    Mockito.when(productItemRepository.getIdsByProductIdAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(PRODUCT_ITEM_IDS);
    oldAndNewPathDomainEventModel1.setNewPath(StringUtils.EMPTY);
    imagePathUpdateDomainEventModel.setImageUpdatedPath(new HashSet<>(
        Collections.singletonList(oldAndNewPathDomainEventModel1)));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel));
    } finally {
      Mockito.verify(productRepository)
          .getIdByStoreIdAndProductCodeAndMarkForDelete(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(productItemRepository).getIdsByProductIdAndMarkForDeleteFalse(Mockito.anyString());
    }
  }

  @Test
   void productAndItemImagePathUpdateProductCodeEmptyTest() {
    imagePathUpdateDomainEventModel.setProductCode(StringUtils.EMPTY);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel));
  }

  @Test
   void productAndItemImagePathUpdateStoreIdEmptyTest() {
    imagePathUpdateDomainEventModel.setStoreId(StringUtils.EMPTY);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel));
  }

  @Test
   void approveProductByVendorTaskFoundTestOk() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE,null,product,null,null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(product);
    Mockito.when(this.productDistributionTaskService
        .getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString()))
        .thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(this.productRepository,  Mockito.times(1)).save(Mockito.any(Product.class));
  }

  @Test
   void approveProductByVendorTaskFoundTestImageNeedCorrectionOk() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE, null, product, null, null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
            this.productDistributionTaskService.getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void approveProductByVendorTaskFoundTestContentNeedCorrectionOk() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE, null, product, null, null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
            this.productDistributionTaskService.getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void approveProductByVendorTaskFoundTestEditedProductOk() throws Exception {
    Product product = createProductList().get(0);
    product.setEdited(true);
    product.setId(PRODUCT_ID);
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), CODE, ReviewType.CONTENT.name())).thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE,null,product,null,null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
            this.productDistributionTaskService.getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString()))
        .thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(pbpFeign)
        .updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), CODE, ReviewType.CONTENT.name());
    Mockito.verify(this.productRepository,  Mockito.times(1)).save(Mockito.any(Product.class));
  }

  @Test
   void approveProductByVendorTaskFoundTestEditedProductSuccessFalse() throws Exception {
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), CODE, ReviewType.CONTENT.name())).thenReturn(new GdnBaseRestResponse(false));
    Product product = createProductList().get(0);
    product.setEdited(true);
    product.setId(PRODUCT_ID);
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE,null,product,null,null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
            this.productDistributionTaskService.getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString()))
        .thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false));
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(pbpFeign)
        .updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), CODE, ReviewType.CONTENT.name());
  }

  @Test
   void approveProductByVendorTaskFoundTestNullNotes() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(new ProductDistributionTask(TASK_CODE, null, product, null, null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(this.productDistributionTaskService
        .getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, StringUtils.EMPTY, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
    Mockito.verify(this.productRepository, Mockito.times(1)).save(Mockito.any(Product.class));
  }

  @Test
   void approveProductWithProductMFDFalseTest() throws Exception {
    Product product = createProductList().get(0);
    product.setCurrentVendor(vendor);
    Mockito.when(this.productDistributionTaskService.findByProductId(PRODUCT_ID)).thenReturn(
        new ProductDistributionTask(TASK_CODE, vendor, product, WorkflowState.IN_REVIEW, null));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(PRODUCT_ID, VENDOR_ID))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findById(PRODUCT_ID)).thenReturn(Optional.of(product));
    Mockito.when(this.productRepository.save(Mockito.any(Product.class))).thenReturn(product);
    this.instance.approveProductByVendor(product, VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval, productReviewer,
      false);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(1)).save(productCaptor.capture());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .updateState(productDistributionTaskArgumentCaptor.capture(), workflowStateArgumentCaptor.capture());
    Assertions.assertFalse(productCaptor.getAllValues().get(0).isMarkForDelete());
    Assertions.assertFalse(
        productDistributionTaskArgumentCaptor.getAllValues().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.PASSED.name(),
        workflowStateArgumentCaptor.getAllValues().get(0).name());
  }

  @Test
   void approveProductByVendorInvalidApproveContent() throws Exception {
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
            .thenReturn(new ProductDistributionTask());
    Mockito.when(
            this.productDistributionTaskService.getWorkflowStatePostApproval(Mockito.anyString(), Mockito.anyString()))
        .thenThrow(new IllegalArgumentException("invalid approve value"));
    Assertions.assertThrows(Exception.class,
      () -> this.instance.approveProductByVendor(createProductList().get(0), VENDOR_CODE, ADDITIONAL_NOTES, isQuickApproval,
        productReviewer, false));
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void rejectProductByVendorTestOk() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    rejectProductDTO.setMerchantCommissionType(CC);

    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
    .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(newProduct.getProductCode());
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
    Mockito.verify(this.xInventoryFeign).getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
  }

  @Test
  void rejectDistributionMappingProductByVendorNegativeTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    productDistributionTask.getProduct().setDistributionMappingStatus(1);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(
      this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
        Mockito.anyString())).thenReturn(productDistributionTask);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    }
  }

  @Test
  void rejectProductByVendorDifferentCommissionTypeTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "TD,TC");
    rejectProductDTO.setMerchantCommissionType(CC);

    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(newProduct.getProductCode());
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
  void rejectProductByVendorInventoryCheckTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    rejectProductDTO.setBulkAction(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xInventoryFeign)
          .getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
      Mockito.verify(this.xBPFeign).filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void rejectProductByVendorInventoryCheckNotBulkAndSchedulerTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(newProduct.getProductCode());
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
  void rejectProductByVendorInventoryCheckProfileResponseFailTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    rejectProductDTO.setSchedulerAction(true);
    rejectProductDTO.setNotes(NOTES);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xBPFeign).filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void rejectProductByVendorInventoryCheckSchedulerTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    rejectProductDTO.setSchedulerAction(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xInventoryFeign)
          .getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
      Mockito.verify(this.xBPFeign).filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void rejectProductByVendorInventoryFailTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    rejectProductDTO.setMerchantCommissionType(CC);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xInventoryFeign)
          .getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
    }
  }

  @Test
  void rejectProductByVendorInventoryCheckConditionTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    rejectProductDTO.setBulkAction(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xBPFeign.filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, REQUEST_ID));
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xInventoryFeign)
          .getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
      Mockito.verify(this.xBPFeign).filterByBusinessPartnerCode(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void rejectProductByVendorInventoryCheckConditionInternalProductTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    ReflectionTestUtils.setField(instance, "warehouseStockCheckSwitch", true);
    ReflectionTestUtils.setField(instance, "warehouseMerchantCommissionTypeList", "CC,TD,TC");
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(true);
    vendor.setAbleToReject(true);
    rejectProductDTO.setBulkAction(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    productDistributionTask.getProduct().setBusinessPartnerCode(Constants.INTERNAL_BUSINESS_PARTNER);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(xInventoryFeign.getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE)).thenReturn(new GdnRestSingleResponse<>(l2StockDetailResponse, REQUEST_ID));
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.xInventoryFeign)
          .getStockDetailsByWarehouseItemSku(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), SKU_CODE);
    }
  }

  @Test
   void rejectProductByVendorPostLiveTrueTestOk() throws Exception {
    vendor.setAbleToReject(true);
    this.newProduct.setPostLive(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
   void rejectPostLiveProductByVendorTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateProductReject", false);
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(anyString(), anyString(), any(), any(), anyString(), any(WorkflowState.class),
            anyString())).thenReturn(new PDTHistoryEventModel());
    newProduct.setPostLive(true);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(newProduct.getId())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode()))
        .thenReturn(productDistributionTask);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn("EVENT");
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode());
    Mockito.verify(this.productRepository).findById(newProduct.getId());
    Mockito.verify(this.productRepository).saveAndFlush(newProduct);
    Mockito.verify(this.productRepository).save(newProduct);
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT),
      anyString(), any());
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaProducer).send(anyString(), anyString(), any(InternalHistoryEventModel.class));
  }

  @Test
   void rejectPreLiveEditedProductByVendorTest() throws Exception {
    newProduct.setPostLive(false);
    newProduct.setEdited(true);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(newProduct.getId())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode()))
        .thenReturn(productDistributionTask);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode());
    Mockito.verify(this.productRepository).findById(newProduct.getId());
    Mockito.verify(this.productRepository).saveAndFlush(newProduct);
    Mockito.verify(this.productRepository).save(newProduct);
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
   void rejectPostLiveProductByVendorPostLiveFalseTest() throws Exception {
    newProduct.setPostLive(false);
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(this.productRepository.findById(newProduct.getId())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode()))
        .thenReturn(productDistributionTask);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatusResponse(PRODUCT_CODE, Collections.singletonList(ACTIVE_STATE), new HashMap<>()));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    } finally {
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(newProduct.getProductCode());
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void rejectProductByVendorTestNotEligibleWorkflowException() throws Exception {
    productWorkflowStatusResponse.setStates(List.of("ACTIVE"));
    Vendor vendor = new Vendor.Builder().vendorCode(VENDOR_CODE).isAbleToReject(true)
        .isQcRequired(false).build();
    this.newProduct.setCurrentVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString()))
      .thenReturn(Optional.of(this.newProduct));
    Mockito.when(
      this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
        Mockito.anyString())).thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString()))
      .thenReturn(productWorkflowStatusResponse);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    Mockito.verify(this.productDistributionTaskService)
      .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
  }

  @Test
   void rejectProductByVendorTestException() throws Exception {
    vendor.setAbleToReject(false);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setProduct(this.newProduct);
    Mockito.when(
      this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
        Mockito.anyString())).thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    Mockito.verify(this.productDistributionTaskService)
      .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
  }

  @Test
  void getAllProductDetailsWithMultipleFilterTest() throws Exception {
    this.instance.getAllProductDetailsWithMultipleFilter(new DistributionTaskMultipleFilterDTO(),
        PageRequest.of(0, 1), STORE_ID);
    Mockito.verify(this.solrVendorCollectionService)
        .getAllProductDetailsWithMultipleFilterSolr(any(DistributionTaskMultipleFilterDTO.class),
            any(Pageable.class), anyString());
  }

  @Test public void findProductBusinessPartnerMapperSearchTestOk() throws Exception {
    Mockito.when(this.solrVendorCollectionService
        .findProductBusinessPartnerMapper(Mockito.anyList(), Mockito.anyString(),
            Mockito.any(Pageable.class), Mockito.anyString())).thenReturn(this.productBusinessPartnerMappers);
    Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers = this.instance
        .findProductBusinessPartnerMapper(WorkflowState.PASSED, "keyWord", this.pageable, true, STORE_ID);
    Mockito.verify(this.solrVendorCollectionService)
        .findProductBusinessPartnerMapper(Mockito.anyList(), Mockito.anyString(),
            Mockito.any(Pageable.class), Mockito.anyString());
    Assertions.assertNotNull(productBusinessPartnerMappers);
  }

  @Test public void findProductBusinessPartnerMapperTestOk() throws Exception {
    Mockito.when(this.solrVendorCollectionService
        .findProductBusinessPartnerMapper(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.anyString())).thenReturn(this.productBusinessPartnerMappers);
    Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers = this.instance
        .findProductBusinessPartnerMapper(WorkflowState.PASSED, "keyWord", this.pageable, false, STORE_ID);
    Mockito.verify(this.solrVendorCollectionService)
        .findProductBusinessPartnerMapper(Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.anyString());
    Assertions.assertNotNull(productBusinessPartnerMappers);
  }

  @Test
   void findProductBusinessPartnerMapperTestNullWorkflowState() throws Exception {
    Mockito.when(this.solrVendorCollectionService
        .findProductBusinessPartnerMapper(Mockito.any(), Mockito.any(),
            Mockito.any(Pageable.class), Mockito.anyString())).thenReturn(this.productBusinessPartnerMappers);
    Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers =
        this.instance.findProductBusinessPartnerMapper(null, "keyWord", this.pageable, false, STORE_ID);
    Mockito.verify(this.solrVendorCollectionService)
        .findProductBusinessPartnerMapper(Mockito.any(), Mockito.any(),
            Mockito.any(Pageable.class), Mockito.anyString());
    Assertions.assertNotNull(productBusinessPartnerMappers);
  }

  @Test
   void findProductBusinessPartnerMapperTestNotOk() throws Exception {
    Mockito.when(this.solrVendorCollectionService
        .findProductBusinessPartnerMapper(Mockito.anyList(), Mockito.anyString(),
            Mockito.any(Pageable.class), Mockito.anyString())).thenReturn(this.productBusinessPartnerMappers);
    Page<ProductBusinessPartnerMapper> productBusinessPartnerMappersResult = this.instance
        .findProductBusinessPartnerMapper(WorkflowState.PASSED, null, this.pageable, true, STORE_ID);
    Mockito.verify(this.solrVendorCollectionService)
        .findProductBusinessPartnerMapper(Mockito.any(), Mockito.any(),
            Mockito.any(Pageable.class), Mockito.anyString());
  }

  private Object[] createReturnVendor(VendorCapacityDTO vendorCapacityDTO) {
    Object[] returnVendor = new Object[6];
    returnVendor[0] = vendorCapacityDTO.getId();
    returnVendor[1] = vendorCapacityDTO.getVendorCode();
    returnVendor[2] = vendorCapacityDTO.getName();
    returnVendor[3] = vendorCapacityDTO.getStartHolidayDate();
    returnVendor[4] = vendorCapacityDTO.getEndHolidayDate();
    returnVendor[5] = vendorCapacityDTO.getRemainingCapacity();
    return returnVendor;
  }

  private List<Object[]> createReturnVendors() {
    List<Object[]> returnVendors = new ArrayList<>();
    VendorCapacityDTO vendorCapacityDTO =
        new VendorCapacityDTO(VENDOR_ID, VENDOR_CODE, VENDOR_NAME, new Date(), new Date(), 100);
    returnVendors.add(createReturnVendor(vendorCapacityDTO));
    vendorCapacityDTO = new VendorCapacityDTO(VENDOR_ID, VENDOR_CODE, VENDOR_NAME, null, null, 100);
    returnVendors.add(createReturnVendor(vendorCapacityDTO));
    vendorCapacityDTO =
        new VendorCapacityDTO(VENDOR_ID, VENDOR_CODE, VENDOR_NAME, new Date(), null, 100);
    returnVendors.add(createReturnVendor(vendorCapacityDTO));
    vendorCapacityDTO =
        new VendorCapacityDTO(VENDOR_ID, VENDOR_CODE, VENDOR_NAME, null, new Date(), 100);
    returnVendors.add(createReturnVendor(vendorCapacityDTO));
    return returnVendors;
  }

  @Test
   void countAllProductDetailsWithMultipleFilterEmptyDistributionTaskFilterDtoTest() throws Exception {
    Boolean includeStatus = true;
    Boolean includeVendors = true;
    Mockito.when(this.vendorRepository.countAllVendorRemainingCapacity()).thenReturn(createReturnVendors());
    Mockito.doThrow(SolrException.class).when(solrVendorCollectionService).getFinalQcCounts(Mockito.anyString());
    Mockito.doThrow(SolrException.class).when(solrVendorCollectionService)
        .getDistributionListCounts(Mockito.any(), Mockito.any());
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.countAllProductDetailsWithMultipleFilter(includeStatus, includeVendors,
          new DistributionTaskMultipleFilterDTO()));
    } finally {
      Mockito.verify(solrVendorCollectionService).getDistributionListCounts(Mockito.any(), Mockito.any());
    }
  }

  @Test
   void countAllProductDetailsExceptionTest() throws Exception {
    Boolean includeStatus = true;
    Boolean includeVendors = true;
    Mockito.when(this.vendorRepository.countAllVendorRemainingCapacity()).thenReturn(createReturnVendors());
    Mockito.doThrow(SolrServerException.class).when(solrVendorCollectionService).getFinalQcCounts(Mockito.anyString());
    Mockito.doThrow(SolrServerException.class).when(solrVendorCollectionService)
        .getDistributionListCounts(Mockito.any(), Mockito.any());
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.countAllProductDetailsWithMultipleFilter(includeStatus, includeVendors,
          new DistributionTaskMultipleFilterDTO()));
    } finally {
      Mockito.verify(solrVendorCollectionService).getDistributionListCounts(Mockito.any(), Mockito.any());
    }
  }

  @Test
   void countAllProductDetailsIOExceptionTest() throws Exception {
    Boolean includeStatus = true;
    Boolean includeVendors = true;
    Mockito.when(this.vendorRepository.countAllVendorRemainingCapacity()).thenReturn(createReturnVendors());
    Mockito.doThrow(IOException.class).when(solrVendorCollectionService).getFinalQcCounts(Mockito.anyString());
    Mockito.doThrow(IOException.class).when(solrVendorCollectionService)
        .getDistributionListCounts(Mockito.any(), Mockito.any());
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.countAllProductDetailsWithMultipleFilter(includeStatus, includeVendors,
          new DistributionTaskMultipleFilterDTO()));
    } finally {
      Mockito.verify(solrVendorCollectionService).getDistributionListCounts(Mockito.any(), Mockito.anyMap());
    }
  }

  @Test
   void countAllProductDetailsWithMultipleFilterEmptyDistributionTaskFilterDtoTest1() throws Exception {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    Mockito.when(this.solrVendorCollectionService.getFinalQcCounts(STORE_ID)).thenReturn(new HashMap<>());
    this.instance.countAllProductDetailsWithMultipleFilter(false, false, distributionTaskMultipleFilterDTO);
    Mockito.verify(solrVendorCollectionService).getFinalQcCounts(STORE_ID);
  }

  @Test
   void countAllProductDetailsWithMultipleFilterEmptyDistributionTaskFilterDtoTest2() throws Exception {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    this.instance.countAllProductDetailsWithMultipleFilter(true, false, distributionTaskMultipleFilterDTO);
    Mockito.verify(solrVendorCollectionService).getDistributionListCounts(STORE_ID, new HashMap<>());
  }

  @Test public void testGetDetailsForAnyProductTypeByCode() throws Exception {
		Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE))
				.thenReturn(this.product);
	    Mockito.doNothing().when(this.productUtils)
        	.initializeProductDetailsWithMFDTrue(Mockito.any());
	    Product productResponse =
	        this.instance.getDetailsForAnyProductTypeByCode(PRODUCT_CODE);
	    Mockito.verify(this.productRepository, Mockito.times(1))
	        .findByProductCode(PRODUCT_CODE);
	    Assertions.assertEquals(this.product, productResponse);
  }

  @Test public void testFindProductStatusForVendor() {

    Vendor vendor = new Vendor.Builder().vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    Date todayStart = DateUtils.addMilliseconds(DateUtils.ceiling(new Date(), Calendar.DATE), 0);
    List<VendorProductStatusDTO> listVendorProductDto = new ArrayList<VendorProductStatusDTO>();
    Mockito.when(this.productRepository.findAllProductStatusForVendor(any(Vendor.class), anyString(), any(Date.class)
    )).thenReturn(listVendorProductDto);
    this.instance.findProductStatusForVendor(vendor, STORE_ID);
    Mockito.verify(this.productRepository).findAllProductStatusForVendor(vendor, STORE_ID,
        DateUtils.addMonths(DateUtils.addDays(todayStart, -1), -1));
  }

  @Test
   void getProductCodeListTest() throws Exception {
    List<String> productCodes = new ArrayList<>();
    productCodes.add("test1");
    productCodes.add("test2");
    Mockito.when(this.productRepository.findProductCodesProductCodesIn(productCodes))
        .thenReturn(productCodes);
    List<String> response = this.instance.getProductCodeList(productCodes);
    Mockito.verify(this.productRepository, Mockito.times(1))
        .findProductCodesProductCodesIn(productCodes);
    Assertions.assertEquals("test1", response.get(0));
    Assertions.assertEquals("test2", response.get(1));
  }

  @Test
   void getBusinessPartnerForVendorTest() {
    this.instance.getBusinessPartnerForVendor(VENDOR_ID, PageRequest.of(1,10));
    Mockito.verify(this.productRepository).findProductBusinessPartnerForVendor(eq(VENDOR_ID), any(Pageable.class));
  }

  @Test
   void rejectProductTest() throws Exception {
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProduct(rejectProductDTO);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
        .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, StringUtils.EMPTY);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
   void rejectProductExceptionTest() throws Exception {
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    this.instance.rejectProduct(rejectProductDTO);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
    Mockito.verify(kafkaProducer).send(com.gdn.mta.domain.event.config.DomainEventName.ADD_PRODUCT_TO_PDT_RETRY,
        rejectProductDTO.getProductCode(),
        new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, rejectProductDTO.getProductCode(),
            Constants.PDT_RETRY_DELETE, StringUtils.EMPTY));
  }

  @Test
   void rejectProductTestException() {
    vendor.setAbleToReject(true);
    this.newProduct.setCurrentVendor(vendor);
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(null);
    try {
      this.instance.rejectProduct(rejectProductDTO);
    } catch (Exception e) {
      Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    }
  }

  @Test
   void getBusinessPartnerListTest_timeFilterTypeFiveDaysAgo() throws IOException, SolrServerException {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    Mockito.when(solrVendorCollectionService
        .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE)))
        .thenThrow(IOException.class);
    Mockito.when(this.productRepository
        .findByStoreIdAndKeywordAndStateAndUpdatedDate(eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class),
            any(Date.class), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(productBusinessPartnerMapperResponseList);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE,
          SIZE));
    } finally {
      Mockito.verify(solrVendorCollectionService)
          .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE));
    }
  }

  @Test
   void getBusinessPartnerListTest_timeFilterTypeThreeToFiveDaysAgo() throws IOException, SolrServerException {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.THREE_TO_FIVE_DAYS_AGO);
    Mockito.when(solrVendorCollectionService
        .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE)))
        .thenThrow(IOException.class);
    Mockito.when(this.productRepository
        .findByStoreIdAndKeywordAndStateAndUpdatedDate(eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class),
            any(Date.class), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(productBusinessPartnerMapperResponseList);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE,
          SIZE));
    } finally {
      Mockito.verify(solrVendorCollectionService)
          .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE));
    }
  }

  @Test
   void getBusinessPartnerListTest_timeFilterTypeTwoDaysAgo() throws IOException, SolrServerException {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TWO_DAYS_AGO);
    Mockito.when(solrVendorCollectionService
        .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE)))
        .thenThrow(IOException.class);
    Mockito.when(this.productRepository
        .findByStoreIdAndKeywordAndStateAndUpdatedDate(eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class),
            any(Date.class), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(productBusinessPartnerMapperResponseList);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE,
          SIZE));
    } finally {
      Mockito.verify(solrVendorCollectionService)
          .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE));
    }
  }

  @Test
   void getBusinessPartnerListTest_statusImagePending() throws IOException, SolrServerException {
    primaryFilterDTO.setContentPending(Boolean.FALSE);
    Mockito.when(solrVendorCollectionService
        .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE)))
        .thenThrow(IOException.class);
    Mockito.when(this.productRepository
        .findByStoreIdAndKeywordAndStateAndUpdatedDate(eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class),
            any(Date.class), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(productBusinessPartnerMapperResponseList);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE,
          SIZE));
    } finally {
      Mockito.verify(solrVendorCollectionService)
          .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE));
    }
  }

  @Test
   void getAssigneeListTest_timeFilterTypeAll() {
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE))).thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_timeFilterTypeFiveDaysAgo() {
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE))).thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_timeFilterTypeThreeToFiveDaysAgo() {
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.THREE_TO_FIVE_DAYS_AGO);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE))).thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_timeFilterTypeTwoDaysAgo() {
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TWO_DAYS_AGO);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE))).thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_timeFilterTypeYesterday() {
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.YESTERDAY);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE))).thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID),
            eq(DEFAULT_ASSIGNEE_KEYWORD), eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE),
            eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_timeFilterTypeToday() {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_statusContentPending() {
    primaryFilterDTO.setImagePending(Boolean.FALSE);
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentPending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentPending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_statusImagePending() {
    primaryFilterDTO.setContentPending(Boolean.FALSE);
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_statusInReview() {
    primaryFilterDTO.setContentPending(Boolean.TRUE);
    primaryFilterDTO.setImagePending(Boolean.TRUE);
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(), any(), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
  }

  @Test
   void getAssigneeListTest_unAssigned() {
    primaryFilterDTO.setAssignment(Boolean.FALSE);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Assertions.assertEquals(List.of(EMPTY_LIST_ELEMENT), response);
  }

  @Test
   void bulkVendorProductActionAssignTest() {
    bulkScreeningProductActionsDTO.setProductCodes(List.of(PRODUCT_CODE));
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(Mockito.anyString())).thenReturn(TASK_CODE);
    Date date = new Date();
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(Mockito.anyList()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(productReviewer));
    instance.bulkVendorProductAction(STORE_ID, date, bulkScreeningProductActionsDTO);
    Mockito.verify(productReviewerService).saveAll(Collections.singletonList(productReviewer));
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(
        List.of(PRODUCT_CODE));
    Mockito.verify(taskHistoryRepository).saveAll(taskHistoryArgumentCaptorList.capture());
    Mockito.verify(this.productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Assertions.assertEquals(PRODUCT_CODE,
        taskHistoryArgumentCaptorList.getAllValues().get(0).get(0).getProductCode());
    Assertions.assertEquals(TASK_CODE,
        taskHistoryArgumentCaptorList.getAllValues().get(0).get(0).getTaskCode());
    Assertions.assertEquals(ASSIGNED_REASON + ASSIGNED_TO + ASSIGNED_BY_TEXT + ASSIGNED_BY,
        taskHistoryArgumentCaptorList.getAllValues().get(0).get(0).getReason());
  }

  @Test
   void bulkVendorProductActionAssignWithEventTest() {
    ReflectionTestUtils.setField(instance,"pdtHistoryUpdateThroughEvent", true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(any(), any(), any(), any(), any(), any(WorkflowState.class),
            any())).thenReturn(new PDTHistoryEventModel());
    bulkScreeningProductActionsDTO.setProductCodes(List.of(PRODUCT_CODE));
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(Mockito.anyString())).thenReturn(TASK_CODE);
    Date date = new Date();
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(Mockito.anyList()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(productReviewer));
    instance.bulkVendorProductAction(STORE_ID, date, bulkScreeningProductActionsDTO);
    Mockito.verify(productReviewerService).saveAll(Collections.singletonList(productReviewer));
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(
        List.of(PRODUCT_CODE));
    Mockito.verify(this.productDistributionTaskRepository).getTaskCodeForProduct(eq(ID));
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT), anyString(), any(PDTHistoryEventModel.class));
  }

  @Test
   void doVendorProductActionTest_forAssigningImageReviewer() throws Exception {
    productHistoryDTOS = new ArrayList<>();
    productHistoryDTOS.add(new ProductHistoryDTO(null, ASSIGNEE, ASSIGN, ASSIGNED_TO));
    productReviewers.get(0).setApproverAssignee(ASSIGN);
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES))
        .thenReturn(productReviewers);
    Mockito.when(productUtils.toJson(Mockito.anyList()))
        .thenReturn(objectMapper.writeValueAsString(productHistoryDTOS));
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product1);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(eq(ID1))).thenReturn(TASK_CODE);
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, ASSIGN, ASSIGNED_TO, new Date());
    Mockito.verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productReviewerService).save(productReviewer);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID1));
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(taskHistoryRepository).save(taskHistoryArgumentCaptor.capture());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(PRODUCT_CODE1, taskHistory.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME1, taskHistory.getProductName());
    Assertions.assertEquals(CATEGORY_CODE, taskHistory.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, taskHistory.getCategoryName());
    Assertions.assertEquals(vendor, taskHistory.getVendor());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, taskHistory.getState());
    Assertions.assertEquals(TASK_CODE, taskHistory.getTaskCode());
    Assertions.assertTrue(taskHistory.getReason().contains(ASSIGNEE));
  }

  @Test
   void doVendorProductActionTest_forReAssigningImageReviewer() throws Exception {
    productReviewers.get(0).setApproverAssignee(StringUtils.EMPTY);
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES))
        .thenReturn(productReviewers);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product1);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(eq(ID1))).thenReturn(TASK_CODE);
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, ASSIGN, ASSIGNED_TO, new Date());
    Mockito.verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productReviewerService).save(productReviewer);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID1));
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(taskHistoryRepository).save(taskHistoryArgumentCaptor.capture());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(PRODUCT_CODE1, taskHistory.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME1, taskHistory.getProductName());
    Assertions.assertEquals(CATEGORY_CODE, taskHistory.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, taskHistory.getCategoryName());
    Assertions.assertEquals(vendor, taskHistory.getVendor());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, taskHistory.getState());
    Assertions.assertEquals(TASK_CODE, taskHistory.getTaskCode());
    Assertions.assertTrue(taskHistory.getReason().contains("Ditugaskan"));
  }

  @Test
   void doVendorProductActionTest_forUnassigningReviewer() throws Exception {
    productReviewers.get(0).setApproverAssignee(ASSIGNEE);
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES))
        .thenReturn(productReviewers);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product1);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(eq(ID1))).thenReturn(TASK_CODE);
    Mockito.when(productUtils.toJson(Mockito.anyList()))
        .thenReturn(objectMapper.writeValueAsString(productHistoryDTOS));
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, UNASSIGN, null, new Date());
    Mockito.verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productReviewerService).save(productReviewer);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID1));
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(taskHistoryRepository).save(taskHistoryArgumentCaptor.capture());
    TaskHistory taskHistory = taskHistoryArgumentCaptor.getValue();
    Assertions.assertEquals(PRODUCT_CODE1, taskHistory.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME1, taskHistory.getProductName());
    Assertions.assertEquals(CATEGORY_CODE, taskHistory.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, taskHistory.getCategoryName());
    Assertions.assertTrue(taskHistory.getReason().contains("Not Assigned"));
  }

  @Test
  void doVendorProductActionTestnoHistory() throws Exception {
    productReviewers.get(0).setApproverAssignee(StringUtils.EMPTY);
    Mockito.when(
        productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_CODES)).thenReturn(productReviewers);
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, UNASSIGN, null,
        new Date());
    Mockito.verify(productReviewerService)
        .findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
  }

  @Test
   void getProductStatusByVendorSolrServerExceptionTest() throws Exception {
    Mockito.when(solrVendorCollectionService.getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false))
        .thenThrow(SolrServerException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenReturn(VENDOR_ID);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductStatusByVendor(STORE_ID, VENDOR_CODE, false, false, false));
    } finally {
      Mockito.verify(solrVendorCollectionService).getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE,
          Boolean.FALSE, false);
    }
  }

  @Test
   void getProductStatusByVendorTest() throws Exception {
    countsMap.put(SolrConstants.TODAY, 10);
    countsMap.put(SolrConstants.YESTERDAY, 9);
    countsMap.put(SolrConstants.TWO_DAYS_AGO, 8);
    countsMap.put(SolrConstants.BRAND_NOT_APPROVED, 11);
    countsMap.put(SolrConstants.MORE_THAN_5_DAYS, 6);
    countsMap.put(SolrConstants.ASSIGNED, 10);
    countsMap.put(SolrConstants.UNASSIGNED, 0);
    Mockito.when(solrVendorCollectionService.getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false))
        .thenReturn(countsMap);
    Map<String, Object> productStatusByVendor = instance.getProductStatusByVendor(STORE_ID, VENDOR_CODE, Boolean.FALSE,
        Boolean.FALSE, false);
    Mockito.verify(solrVendorCollectionService).getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE,
        Boolean.FALSE, false);
    Assertions.assertEquals(10, productStatusByVendor.get(TODAY));
    Assertions.assertEquals(10, productStatusByVendor.get(ASSIGNED));
    Assertions.assertEquals(0, productStatusByVendor.get(UNASSIGNED));
    Assertions.assertEquals(7, productStatusByVendor.size());
    Assertions.assertEquals(11, productStatusByVendor.get(BRAND_NOT_APPROVED));
  }

  @Test
   void getProductStatusByVendorSolrExceptionTest() throws Exception {
    Mockito.when(solrVendorCollectionService.getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false))
        .thenThrow(SolrException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenReturn(VENDOR_ID);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductStatusByVendor(STORE_ID, VENDOR_CODE, false, false,
        false));
    } finally {
      Mockito.verify(solrVendorCollectionService).getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false);
    }
  }

  @Test
   void getProductStatusByVendorIOExceptionTest() throws Exception {
    Mockito.when(solrVendorCollectionService.getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false))
        .thenThrow(IOException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenReturn(VENDOR_ID);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductStatusByVendor(STORE_ID, VENDOR_CODE, false, false,
            false));
    } finally {
      Mockito.verify(solrVendorCollectionService).getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false);
    }
  }

  @Test
   void getProductStatusByVendorExceptionTest() throws Exception {
    Mockito.when(solrVendorCollectionService.getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        false))
        .thenThrow(IOException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductStatusByVendor(STORE_ID, VENDOR_CODE, false, false, false));
    } finally {
      Mockito.verify(solrVendorCollectionService).getFilterCounts(STORE_ID, VENDOR_CODE, Boolean.FALSE,
          Boolean.FALSE, false);
    }
  }

  @Test
   void getReviewProductCountByVendorTest() throws Exception {
    Mockito
        .when(this.solrVendorCollectionService.getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE))
        .thenReturn(new HashMap<>());
    Map<String, Object> productStatusByVendor = instance.getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE);
    Mockito.verify(this.solrVendorCollectionService)
        .getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE);
  }

  @Test
   void getReviewProductCountByVendorExceptionTest() throws Exception {
    Mockito
        .when(this.solrVendorCollectionService.getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE))
        .thenThrow(SolrException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE));
    } finally {
      Mockito.verify(this.solrVendorCollectionService)
          .getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE);
    }
  }

  @Test
   void getProductListTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap();
    productReviewerMap.put(PRODUCT_CODE, new ProductReviewer());
    Mockito.when(this.solrVendorCollectionService
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES),
          any(Pageable.class))).thenReturn(productAndReviewerDetailsDTOS);
    Mockito.when(productReviewerService.findProductReviewerMapByProductCodes(eq(STORE_ID),
        anyList())).thenReturn(productReviewerMap);
    Page<ProductAndReviewerDetailsDTO> response =
      this.instance.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE);
    Mockito.verify(this.solrVendorCollectionService)
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class));
    Assertions.assertEquals(CATEGORY_CODE,
        response.getContent().get(0).getProduct().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        response.getContent().get(0).getProduct().getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME,
        response.getContent().get(0).getProduct().getBusinessPartnerName());
    Assertions.assertEquals(STORE_ID, response.getContent().get(0).getProduct().getStoreId());
  }

  @Test
   void getProductListDiffProductCodeTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap();
    productReviewerMap.put(PRODUCT_CODE1, new ProductReviewer());
    Mockito.when(this.solrVendorCollectionService
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES),
            any(Pageable.class))).thenReturn(productAndReviewerDetailsDTOS);
    Mockito.when(productReviewerService.findProductReviewerMapByProductCodes(eq(STORE_ID),
        anyList())).thenReturn(productReviewerMap);
    Page<ProductAndReviewerDetailsDTO> response =
        this.instance.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE);
    Mockito.verify(this.solrVendorCollectionService)
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class));
    Assertions.assertEquals(CATEGORY_CODE,
        response.getContent().get(0).getProduct().getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        response.getContent().get(0).getProduct().getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME,
        response.getContent().get(0).getProduct().getBusinessPartnerName());
    Assertions.assertEquals(STORE_ID, response.getContent().get(0).getProduct().getStoreId());
  }

  @Test
   void getProductListTest_withCnCategory() throws Exception {
    summaryFilterDTO.setIsCnCategory(Boolean.FALSE);
    Mockito.when(this.solrVendorCollectionService
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class)))
        .thenThrow(Exception.class);
    Mockito.when(this.productRepository
        .findProductByStoreIdAndKeywordAndStateAndUpdatedDateAndCategoryAndBusinessPartnerCodeAndAssigneeEmailId(
            eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class), any(Date.class),
            eq(categoryCodes), eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_ASSIGNEE_EMAIL_ID),
            eq(DEFAULT_SORT_METHOD), eq(null), any(Pageable.class), eq(VENDOR_CODE),
            eq(Boolean.FALSE), eq(IMAGE_VIOLATION), eq(Boolean.TRUE))).thenReturn(productPage1);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE
          , SIZE));
    } finally {
      Mockito.verify(this.solrVendorCollectionService)
          .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class));
    }
  }

  @Test
   void getProductList_expectExceptionOnFetchingChildCategory() throws Exception {
    Mockito.when(this.productRepository
        .findProductByStoreIdAndKeywordAndStateAndUpdatedDateAndCategoryAndBusinessPartnerCodeAndAssigneeEmailId(
            eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class), any(Date.class),
            eq(List.of(CATEGORY_CODE)), eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_ASSIGNEE_EMAIL_ID),
            eq(DEFAULT_SORT_METHOD), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.FALSE),
            eq(IMAGE_VIOLATION), eq(Boolean.TRUE)))
        .thenReturn(productPage1);
    Mockito.when(this.solrVendorCollectionService
        .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class)))
        .thenThrow(Exception.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE
          , SIZE));
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(this.solrVendorCollectionService)
          .getVendorProductsList(eq(STORE_ID), eq(summaryFilterDTO), eq(STATES), any(Pageable.class));

    }
  }

  @Test
   void updateAndApproveProductTest_Image() throws Exception {
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW,
            new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(product, product);
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(2)).save(product);
    Mockito.verify(this.productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());

  }

  @Test
   void updateAndApproveProductTest_ImageWithNotes() throws Exception {
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW,
        new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setVideoUrl(YOUTUBE_URL);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(this.productUtils.validateYouTubeUrl(Mockito.anyString(),Mockito.any())).thenReturn(true);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(product, product);
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(2)).save(Mockito.any(Product.class));
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
  }

  @Test
   void updateAndApproveProductTest_SameUrl() throws Exception {
    ReflectionTestUtils.setField(instance, "brandExistCheckEnabled", true);
    ReflectionTestUtils.setField(instance, "decrementVendorQuotaCounter", true);
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW,
        new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setVideoUrl(YOUTUBE_URL);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(Mockito.any(), Mockito.any()))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(Mockito.any(), Mockito.any(), Mockito.anyBoolean())).thenReturn(product);
    Mockito.when(this.productUtils.validateYouTubeUrl(Mockito.anyString(),Mockito.any())).thenReturn(true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(anyString(), anyString(), any(), any(), anyString(), any(WorkflowState.class),
            anyString())).thenReturn(new PDTHistoryEventModel());
    Mockito.when(productUtils.getProductDetailChanges(Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductHistoryDTO()));
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
     this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(Mockito.any(), Mockito.any());
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productDistributionTaskRepository).getTaskCodeForProduct(product.getId());
    Mockito.verify(this.productRepository, Mockito.times(2)).save(Mockito.any(Product.class));
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
    Mockito.verify(kafkaProducer, times(1))
      .send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT), anyString(), any());
    Mockito.verify(vendorQuotaCounterRepository).decrementInProgressQuota(product.getCurrentVendor(), 1);
  }

  @Test
   void updateAndApproveProductTest() throws Exception {
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW,
        new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setVideoUrl(YOUTUBE_URL);
    product.setBrandCode(null);
    product.setMarkForDelete(true);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.doNothing().when(this.productUtils).initializeAllProductDetails(Mockito.any(Product.class));
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(Mockito.any(), Mockito.any()))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(Mockito.any(), Mockito.any(), Mockito.anyBoolean())).thenReturn(product);
    Mockito.when(this.productUtils.validateYouTubeUrl(Mockito.anyString(),Mockito.any())).thenReturn(true);
    Mockito.when(productUtils.getProductDetailChanges(Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductHistoryDTO()));
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(Mockito.any(), Mockito.any());
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productDistributionTaskRepository).getTaskCodeForProduct(product.getId());
    Mockito.verify(this.taskHistoryRepository).saveAndFlush(taskHistoryArgumentCaptor.capture());
    Mockito.verify(this.productRepository, Mockito.times(2)).save(Mockito.any(Product.class));
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productUtils).getBrandCodeByBrandName(BRAND_NAME);
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
  }

  @Test
   void updateAndApproveProductTestWithHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    Mockito.when(taskHistoryService
        .generatePDTHistoryEventModel(anyString(), anyString(), any(), any(), anyString(), any(WorkflowState.class),
            anyString())).thenReturn(new PDTHistoryEventModel());
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW,
        new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setVideoUrl(YOUTUBE_URL);
    product.setBrandCode(null);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.doNothing().when(this.productUtils).initializeAllProductDetails(Mockito.any(Product.class));
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(Mockito.any(), Mockito.any()))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(Mockito.any(), Mockito.any(), Mockito.anyBoolean())).thenReturn(product);
    Mockito.when(this.productUtils.validateYouTubeUrl(Mockito.anyString(),Mockito.any())).thenReturn(true);
    Mockito.when(productUtils.getProductDetailChanges(Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(new ProductHistoryDTO()));
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(Mockito.any(), Mockito.any());
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productDistributionTaskRepository).getTaskCodeForProduct(product.getId());
    Mockito.verify(this.productRepository, Mockito.times(2)).save(Mockito.any(Product.class));
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productUtils).getBrandCodeByBrandName(BRAND_NAME);
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
    Mockito.verify(kafkaProducer, times(1))
      .send(eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT), anyString(), any());
  }

  @Test
   void updateAndApproveProductInvalidUrlTest() throws Exception {
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask(TASK_CODE, new Vendor(), this.product, WorkflowState.IN_REVIEW, new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setVideoUrl(YOUTUBE_URL);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(this.productUtils.validateYouTubeUrl(Mockito.anyString(), Mockito.any())).thenReturn(false);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateAndApproveProduct(VENDOR_CODE, createProductList().get(1), ADDITIONAL_NOTES, false,
          productReviewer));
    } finally {
      Mockito.verify(this.productUtils).initializeAllProductDetails(product);
      Mockito.verify(this.productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    }
  }

  @Test
   void updateAndApproveProductTest_ImageWithNotes1() throws Exception {
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW,
        new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(this.productUtils).replaceProductImageDetails(product, product);
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository, Mockito.times(2)).save(Mockito.any(Product.class));
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
  }

  @Test
   void updateAndApproveProductTest_Content() throws Exception {
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.IN_REVIEW, new Date());
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product))
        .thenReturn(product);
    Mockito.when(pbpFeign
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    Mockito.verify(pbpFeign)
        .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    Mockito.verify(this.productUtils).replaceProductImageDetails(product, product);
    Mockito.verify(this.productUtils, Mockito.times(2)).regenerateProductImageDetails(product);
    Mockito.verify(this.productUtils).replaceProductDetails(product, product, true);
    Mockito.verify(this.productUtils).initializeAllProductDetails(product);
    Mockito.verify(this.productDistributionTaskService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productRepository)
        .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    Mockito.verify(this.productRepository, Mockito.times(2)).save(product);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(eq(DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT), anyString(), any());
  }

  @Test
   void updateAndApproveProductTest_productStatePassed() throws Exception {
    ProductDistributionTask productDistributionTask = new ProductDistributionTask(TASK_CODE,
        new Vendor(), this.product, WorkflowState.PASSED, new Date());
    Product product = createProductList().get(0);
    product.setState(WorkflowState.PASSED);
    product.setId(PRODUCT_ID);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false
          , productReviewer));
    } finally {
      Mockito.verify(this.productRepository)
          .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    }
  }

  @Test
   void updateAndApproveProductTest_IncorrectAssignee() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    productReviewer.setApproverAssignee(USERNAME);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(productReviewer);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    try {
      this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.productRepository)
          .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
    }
  }

  @Test
   void updateAndApproveProductTest_ProtectedBrand() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(productReviewer);
    Mockito.when(pbpFeign
            .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(productUtils.validateProtectedBrand(Mockito.anyString(), Mockito.any())).thenReturn(false);
    try {
      this.instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.productRepository)
          .findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE.toLowerCase());
      Mockito.verify(pbpFeign)
          .findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              Constants.YOUTUBE_URL_VALIDATION_SWITCH);
      Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    }
  }

  @Test
   void doProductContentNeedForCorrectionTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME,
            PRODUCT_CODE,
            new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService).updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentNeedForCorrectionExceptionTest() {
    product.setState(WorkflowState.PASSED);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext).getBean(ProductService.class);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentNeedForCorrectionImageNotesEligibleTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    String productNotes = "{\"contentAdditionalNotes\":\"content\",\"allVariants\":false}";
    product.setProductNotes(productNotes);
    ProductNotesResponse productNotesResponse = ProductNotesResponse.builder().contentAdditionalNotes("content").allVariants(true).build();
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(mapper.readValue(product.getProductNotes(), ProductNotesResponse.class))
        .thenReturn(productNotesResponse);
    Mockito.when(mapper.writeValueAsString(productNotesResponse)).thenReturn(productNotes);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(mapper).readValue(product.getProductNotes(), ProductNotesResponse.class);
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductImageNeedForCorrectionTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.IMAGE.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertTrue(response.isImageNeedCorrection());
  }

  @Test
   void doProductImageNeedForCorrectionInValidTypeTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(REQUEST_ID);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext).getBean(ProductService.class);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentAndImageNeedForCorrectionEligibleTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT_AND_IMAGE.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertTrue(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentAndImageNeedForCorrectionNotEligibleTest() {
    product.setState(WorkflowState.PASSED);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT_AND_IMAGE.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext).getBean(ProductService.class);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentNeedForCorrectionNonNulItemsTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentNeedForCorrectionNonNulNotesTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    productItem.setSkuCode(SKU_CODE);
    ItemNotesRequest itemNotesRequest = new ItemNotesRequest();
    itemNotesRequest.setItemName(REQUEST_ID);
    needRevisionRequest.setItemNotes(Collections.singletonList(itemNotesRequest));
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext,Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper, Mockito.times(2)).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductContentNeedForCorrectionItemNotesTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    productItem.setSkuCode(SKU_CODE);
    ItemNotesRequest itemNotesRequest = new ItemNotesRequest();
    itemNotesRequest.setSkuCode(SKU_CODE);
    needRevisionRequest.setItemNotes(Collections.singletonList(itemNotesRequest));
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.IMAGE.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(mapper.writeValueAsString(itemNotesRequest))
        .thenReturn(objectMapper.writeValueAsString(itemNotesRequest));
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext,Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper, Mockito.times(2)).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertTrue(response.isImageNeedCorrection());
    Assertions.assertEquals(objectMapper.writeValueAsString(itemNotesRequest),
        productListCaptor.getValue().get(0).getProductItems().get(0).getItemNotes());
  }

  @Test
   void doProductContentNeedForCorrectionSkuCodeTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    productItem.setSkuCode(SKU_CODE);
    ItemNotesRequest itemNotesRequest = new ItemNotesRequest();
    itemNotesRequest.setSkuCode(SKU_CODE);
    itemNotesRequest.setVendorNotes(Collections.singletonList(EMAIL_ADDRESS));
    needRevisionRequest.setItemNotes(Collections.singletonList(itemNotesRequest));
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(vendor);
    needRevisionRequest.setNeedRevisionType(NeedRevisionType.CONTENT.name());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    NeedRevisionResponse response =
        instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USERNAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext,Mockito.times(2)).getBean(ProductService.class);
    Mockito.verify(productRepository).saveAll(productListCaptor.capture());
    Mockito.verify(mapper, Mockito.times(2)).writeValueAsString(objectArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, PRODUCT_CODE,
        new PDTNeedRevisionEventModel(STORE_ID, PRODUCT_CODE));
    Mockito.verify(solrVendorCollectionService).deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService)
        .updateState(null, WorkflowState.NEED_CORRECTION);
    Assertions.assertTrue(productListCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION,
        productListCaptor.getValue().get(0).getState());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void deleteProductsTest() {
    PRODUCT_CODES.clear();
    PRODUCT_CODES.add(PRODUCT_CODE1);
    PRODUCT_IDS.clear();
    PRODUCT_IDS.add(ID1);
    PRODUCT_ITEM_IDS.clear();
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID1);
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID2);
    Mockito.doNothing().when(this.productAttributeRespository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.productImageRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.productDistributionTaskRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.when(productItemRepository.findByProductIds(Mockito.eq(PRODUCT_IDS), Mockito.eq(pageRequest1)))
        .thenReturn(new PageImpl<>(PRODUCT_ITEM_IDS, pageRequest1, 2));
    Mockito.doNothing().when(this.productItemImageRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    Mockito.doNothing().when(this.productItemAttributeRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    Mockito.doNothing().when(this.productItemRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.taskHistoryRepository).deletebyProductCodes(STORE_ID, PRODUCT_CODES);
    Mockito.doNothing().when(this.productRepository).deleteById(PRODUCT_IDS);

    this.instance.deleteProducts(STORE_ID, PRODUCT_IDS, PRODUCT_CODES, 2);

    Mockito.verify(productAttributeRespository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productImageRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productDistributionTaskRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productItemRepository).findByProductIds(PRODUCT_IDS, pageRequest1);
    Mockito.verify(productItemImageRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    Mockito.verify(productItemAttributeRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    Mockito.verify(productItemRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(taskHistoryRepository).deletebyProductCodes(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productRepository).deleteById(PRODUCT_IDS);
    Mockito.verify(this.productRepository).deleteById(PRODUCT_IDS);
    Mockito.verify(this.productReviewerService).deleteByProductCodesIn(PRODUCT_CODES);
  }

  @Test
   void deleteProductsExceptionTest() {
    PRODUCT_IDS.clear();
    PRODUCT_IDS.add(ID1);
    PRODUCT_ITEM_IDS.clear();
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID1);
    PRODUCT_ITEM_IDS.add(PRODUCT_ITEM_ID2);
    Mockito.doNothing().when(this.productImageRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.when(productItemRepository.findByProductIds(Mockito.eq(PRODUCT_IDS), Mockito.eq(pageRequest1)))
        .thenReturn(new PageImpl<>(PRODUCT_ITEM_IDS, pageRequest1, 2));
    Mockito.doThrow(RuntimeException.class).when(this.productItemImageRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    try {
      this.instance.deleteProducts(STORE_ID, PRODUCT_IDS, PRODUCT_CODES, 2);
    } catch (Exception e) {
      Mockito.verify(productItemRepository).findByProductIds(PRODUCT_IDS, pageRequest1);
      Mockito.verify(productItemImageRepository).deleteByProductItemIds(PRODUCT_ITEM_IDS);
    }
  }

  @Test
   void deleteProductsEmptyItemTest() {
    PRODUCT_CODES.clear();
    PRODUCT_CODES.add(PRODUCT_CODE1);
    PRODUCT_IDS.clear();
    PRODUCT_IDS.add(ID1);
    PRODUCT_ITEM_IDS.clear();
    Mockito.doNothing().when(this.productAttributeRespository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.productImageRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.productDistributionTaskRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.when(productItemRepository.findByProductIds(Mockito.eq(PRODUCT_IDS), Mockito.eq(pageRequest2)))
        .thenReturn(new PageImpl<>(Collections.emptyList(), pageRequest2, 0));
    Mockito.doNothing().when(this.productItemRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.doNothing().when(this.taskHistoryRepository).deletebyProductCodes(STORE_ID, PRODUCT_CODES);
    Mockito.doNothing().when(this.productRepository).deleteById(PRODUCT_IDS);
    this.instance.deleteProducts(STORE_ID, PRODUCT_IDS, PRODUCT_CODES, 1);
    Mockito.verify(productAttributeRespository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productImageRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productDistributionTaskRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(productItemRepository).findByProductIds(PRODUCT_IDS, pageRequest2);
    Mockito.verify(productItemRepository).deleteByProductIds(PRODUCT_IDS);
    Mockito.verify(taskHistoryRepository).deletebyProductCodes(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productRepository).deleteById(PRODUCT_IDS);
    Mockito.verify(this.productRepository).deleteById(PRODUCT_IDS);
    Mockito.verify(this.productReviewerService).deleteByProductCodesIn(PRODUCT_CODES);
  }

  @Test
   void doProductNeedCorrectionVendorFailedTest() {
    vendor.setVendorCode(VENDOR_CODE1);
    product.setProductCode(PRODUCT_CODE);
    product.setCurrentVendor(vendor);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    NeedRevisionResponse
        response = this.instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext).getBean(ProductService.class);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());
  }

  @Test
   void doProductNeedCorrectionProductStateValidationTest() {
    vendor.setVendorCode(VENDOR_CODE1);
    product.setCurrentVendor(vendor);
    product.setState(WorkflowState.PASSED);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(instance);
    NeedRevisionResponse
        response = this.instance.doProductNeedForCorrection(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, needRevisionRequest);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(applicationContext).getBean(ProductService.class);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertFalse(response.isContentNeedCorrection());
    Assertions.assertFalse(response.isImageNeedCorrection());

  }

  @Test
   void getAssigneeListTest_filterByKeywordWith2Elements() {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    primaryFilterDTO.setKeyword(DEFAULT_ASSIGNEE_KEYWORD);
    assigneeResponseList.add(DEFAULT_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(DEFAULT_ASSIGNEE_KEYWORD),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
    Assertions.assertEquals(ASSIGNEE_LIST_SIZE, response.size());
  }

  @Test
   void getAssigneeListTest_filterByKeywordWithEmptyKeyword() {
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    primaryFilterDTO.setKeyword(StringUtils.EMPTY);
    assigneeResponseList.add(DEFAULT_KEYWORD);
    Mockito.when(this.productRepository
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(StringUtils.EMPTY),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(assigneeResponseList);
    List<String> response = this.instance.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
    Mockito.verify(this.productRepository)
        .findAssigneeByStoreIdAndKeywordAndStateAndUpdatedDateContentAndImagePending(eq(STORE_ID), eq(StringUtils.EMPTY),
            eq(STATES), any(Date.class), any(Date.class), eq(null), eq(VENDOR_CODE), eq(Boolean.TRUE));
    Assertions.assertEquals(DEFAULT_ASSIGNEE_EMAIL_ID, response.get(0));
    Assertions.assertEquals(DEFAULT_KEYWORD, response.get(1));
  }

  @Test
   void getBusinessPartnerListTest_withInternalBusinessPartnerCode() throws IOException, SolrServerException {
    productBusinessPartnerMapperResponse = new ProductBusinessPartnerMapperResponse(INTERNAL, INTERNAL);
    productBusinessPartnerMapperResponseList.clear();
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    Mockito.when(solrVendorCollectionService
        .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE)))
        .thenThrow(IOException.class);
    Mockito.when(this.productRepository
        .findByStoreIdAndKeywordAndStateAndUpdatedDate(eq(STORE_ID), eq(DEFAULT_KEYWORD), eq(STATES), any(Date.class),
            any(Date.class), eq(null), any(Pageable.class), eq(VENDOR_CODE), eq(Boolean.TRUE)))
        .thenReturn(productBusinessPartnerMapperResponseList);
    try {
      Assertions.assertThrows(Exception.class,
        () -> this.instance.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE,
          SIZE));
    } finally {
      Mockito.verify(solrVendorCollectionService)
          .getBusinessPartnerList(eq(STORE_ID), eq(primaryFilterDTO), eq(STATES), eq(PAGE), eq(SIZE));
    }
  }

  @Test
   void sendProductBackToVendorNotEmptyWorkflowStatusTest() throws Exception {
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.sendProductBackToVendor(PRODUCT_CODE));
    } finally {
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void sendProductBackToVendorNotEmptyStatusTest() throws Exception {
    productWorkflowStatus =
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<>(Collections.singletonList(IN_VENDOR_STATE)),
            null);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(productWorkflowStatus);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.sendProductBackToVendor(PRODUCT_CODE));
    } finally {
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void sendProductBackToVendorNoVendorKeyTest() throws Exception {
    productWorkflowStatus =
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<>(Collections.singletonList(ACTIVE_STATE)),
            status);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(productWorkflowStatus);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.sendProductBackToVendor(PRODUCT_CODE));
    } finally {
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void sendProductBackToVendorNotInVendorTest() throws Exception {
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<>(Collections.singletonList(Constants.DRAFT)),
            status));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.sendProductBackToVendor(PRODUCT_CODE));
    } finally {
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void sendProductBackToVendorActiveProductTest() throws Exception {
    productWorkflowStatus =
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<>(Collections.singletonList(ACTIVE_STATE)),
            status);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(productWorkflowStatus);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.sendProductBackToVendor(PRODUCT_CODE));
    } finally {
      Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    }
  }

  @Test
   void sendProductBackToVendorTest() throws Exception {
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(productWorkflowStatus);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    instance.sendProductBackToVendor(PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).clearPresentDistributionTaskAndCreateNewTask(productArgumentCaptor.capture());
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Assertions.assertEquals(WorkflowState.IN_REVIEW,
        productArgumentCaptor.getAllValues().get(0).getState());
    Assertions.assertEquals(WorkflowState.IN_REVIEW,
        productArgumentCaptor.getAllValues().get(1).getState());
    Assertions.assertEquals(0, productArgumentCaptor.getAllValues().get(0).getQcRetryCount());
    Assertions.assertEquals(0, productArgumentCaptor.getAllValues().get(1).getQcRetryCount());
    Assertions.assertEquals(Boolean.FALSE,
        productArgumentCaptor.getAllValues().get(0).isMarkForDelete());
    Assertions.assertEquals(Boolean.FALSE,
        productArgumentCaptor.getAllValues().get(1).isMarkForDelete());
  }

  @Test
   void sendProductBackToVendorPostLive() throws Exception {
    productWorkflowStatus.setStates(Collections.singletonList(ACTIVE_STATE));
    productWorkflowStatus.setReviewPending(true);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(PRODUCT_CODE)).thenReturn(productWorkflowStatus);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    instance.sendProductBackToVendor(PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).clearPresentDistributionTaskAndCreateNewTask(productArgumentCaptor.capture());
    Mockito.verify(productRepository).save(productArgumentCaptor.capture());
    Assertions.assertEquals(WorkflowState.IN_REVIEW,
        productArgumentCaptor.getAllValues().get(0).getState());
    Assertions.assertEquals(WorkflowState.IN_REVIEW,
        productArgumentCaptor.getAllValues().get(1).getState());
    Assertions.assertEquals(0, productArgumentCaptor.getAllValues().get(0).getQcRetryCount());
    Assertions.assertEquals(0, productArgumentCaptor.getAllValues().get(1).getQcRetryCount());
    Assertions.assertEquals(Boolean.FALSE,
        productArgumentCaptor.getAllValues().get(0).isMarkForDelete());
    Assertions.assertEquals(Boolean.FALSE,
        productArgumentCaptor.getAllValues().get(1).isMarkForDelete());
  }

  @Test
   void republishFinalQcProductsForApprovalTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(productList.get(0), false);
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
  }

  @Test
   void republishFinalQcProductsForApprovalExceptionTest() throws Exception {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.doThrow(RuntimeException.class).when(productRepository).getProductsAboveQcRetryCount(5);
    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> instance.republishFinalQcProductsForApproval(5, 2, 100));
    } finally {
      Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
      Mockito.verify(productRepository)
          .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
              Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
      Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
      Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
      Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
      Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    }
  }

  @Test
   void republishFinalQcProductsForApprovalEmptyProductsTest() {
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
  }

  @Test
   void republishFinalQcProductsForApprovalWhenProductActiveAndReviewPendingFalse() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.FALSE);
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE_1);
    pdtProduct.setMarkForDelete(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    Mockito.when(
        productRepository.findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
            eq(5), Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1))
        .thenReturn(prdProductResponses);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(pdtProduct);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository).save(Mockito.any(Product.class));
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWhenProductIsTakenDownAndReviewPendingFalse() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.FALSE);
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE_1);
    pdtProduct.setMarkForDelete(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.TRUE);
    prdProductResponse.setTakenDown(Boolean.TRUE);
    prdProductResponse.setProductSku(PRODUCT_SKU);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    ItemLevel4ListingWebRequest itemLevel4ListingWebRequest = new ItemLevel4ListingWebRequest();
    itemLevel4ListingWebRequest.setProductSkus(new HashSet<>(Collections.singletonList(PRODUCT_SKU)));
    Mockito.when(
        productRepository.findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
            eq(5), Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1))
        .thenReturn(prdProductResponses);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(pdtProduct);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productServiceRepository)
        .processProductVendorSearchAutoHeal(GdnMandatoryRequestParameterUtil.getStoreId(), PRODUCT_CODE_1);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository).save(Mockito.any(Product.class));
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWhenProductIsTakenDownAndReviewPendingTrue() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE_1);
    pdtProduct.setMarkForDelete(Boolean.FALSE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.TRUE);
    prdProductResponse.setTakenDown(Boolean.TRUE);
    prdProductResponse.setProductSku(PRODUCT_SKU);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(
        productRepository.findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
            eq(5), Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(productList.get(0), false);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWhenReviewPendingFalseAndXproductResponseEmpty() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.FALSE);
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE_1);
    pdtProduct.setMarkForDelete(Boolean.FALSE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(
        productRepository.findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
            eq(5), Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1))
        .thenReturn(prdProductResponses);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWhenReviewPendingFalseAndTakenDownFalse() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.FALSE);
    Product pdtProduct = new Product();
    pdtProduct.setProductCode(PRODUCT_CODE_1);
    pdtProduct.setMarkForDelete(Boolean.FALSE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.TRUE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponse.setProductSku(PRODUCT_SKU);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(
        productRepository.findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(
            eq(5), Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1))
        .thenReturn(prdProductResponses);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(productList.get(0), false);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWithEmailTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(productList.get(0), false);
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EMAIL_ADDRESS,
        messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalWithEmailExceptionTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    Mockito.doNothing().when(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(productList.get(0), false);
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertNotNull(
        messageEmailRequestArgumentCaptor.getValue().getVariables().get(EMAIL_OBJECT));
  }

  @Test
   void republishFinalQcProductsForApprovalEditedProductTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    productList.get(0).setEdited(true);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
  }

  @Test
   void republishFinalQcProductsForApprovalEditedProductExceptionTest() {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productList.get(0).setEdited(true);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
  }

  @Test
   void republishFinalQcProductsForApprovalRevisedProductTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    productList.get(0).setRevised(true);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1)).thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
    Mockito.when(productRepository
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)))).thenReturn(productList);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1)).thenReturn(productDetailResponse);
    instance.republishFinalQcProductsForApproval(5, 2, 100);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productRepository)
        .findProductCodeByStatePassedAndUpdatedDateLessThanAndMarkForDeleteFalseAndQcRetryCountLessThan(eq(5),
            Mockito.any(Date.class), eq(PageRequest.of(0, 100)));
    Mockito.verify(productUtils).initializeAllProductDetails(productList.get(0));
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(productList.get(0), false);
    Mockito.verify(productRepository).updateQcRetryCount(productList.get(0).getProductCode());
    Mockito.verify(productRepository).getProductsAboveQcRetryCount(5);
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository).clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID,PRODUCT_CODE_1);
  }

  @Test
   void retryFinalQCProductsTest() throws Exception {
    ProductWorkflowStatusResponse productDetailResponse = new ProductWorkflowStatusResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE_1);
    productDetailResponse.setReviewPending(Boolean.TRUE);
    List<PrdProductResponse> prdProductResponses = new ArrayList<>();
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponses.add(prdProductResponse);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    productResponse.setReviewPending(Boolean.TRUE);
    product.setState(WorkflowState.PASSED);
    product.setQcRetryCount(5);
    product.setProductCode(PRODUCT_CODE_1);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE_1)).thenReturn(product);
    StuckProductsDTO stuckProductsDTO = new StuckProductsDTO();
    List<StuckProductsDTO> stuckProducts = new ArrayList<>();
    stuckProducts.add(stuckProductsDTO);
    Mockito.when(productServiceRepository.getProductBasicDetailByProductCode(PRODUCT_CODE_1))
        .thenReturn(productResponse);
    Mockito.doNothing().when(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.when(productRepository.getProductsAboveQcRetryCount(5)).thenReturn(stuckProducts);
    Mockito.when(productWorkflowRepository.getWorkflowStatus(PRODUCT_CODE_1))
        .thenReturn(productDetailResponse);
    instance.retryFinalQCProducts(PRODUCT_CODE_1);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productWorkflowRepository).getWorkflowStatus(PRODUCT_CODE_1);
    Mockito.verify(productUtils).initializeAllProductDetails(product);
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(product, false);
    Mockito.verify(productRepository).updateQcRetryCount(product.getProductCode());
    Mockito.verify(productServiceRepository).getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(productServiceRepository)
        .clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
  }

  @Test
   void retryFinalQCProductsStateTest() throws Exception {
    product.setState(WorkflowState.IN_REVIEW);
    product.setQcRetryCount(5);
    product.setProductCode(PRODUCT_CODE_1);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE_1)).thenReturn(product);
    instance.retryFinalQCProducts(PRODUCT_CODE_1);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE_1);
  }

  @Test
   void retryFinalQCProductsRetryCountTest() throws Exception {
    product.setState(WorkflowState.PASSED);
    product.setQcRetryCount(3);
    product.setProductCode(PRODUCT_CODE_1);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE_1)).thenReturn(product);
    instance.retryFinalQCProducts(PRODUCT_CODE_1);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE_1);
  }

  @Test
   void retryFinalQCProductsMarkForDeleteTest() throws Exception {
    product.setState(WorkflowState.PASSED);
    product.setMarkForDelete(true);
    product.setQcRetryCount(5);
    product.setProductCode(PRODUCT_CODE_1);
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE_1)).thenReturn(product);
    instance.retryFinalQCProducts(PRODUCT_CODE_1);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE_1);
  }

  @Test
   void updateProductAsPostLiveTrueTest() throws Exception {
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    instance.updateProductAsPostLiveTrue(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).initializeAllProductDetails(product);
    Mockito.verify(productRepository).updateProductPostLiveFlag(PRODUCT_CODE, true);
    Mockito.verify(approvedProductPublisherService).publishVendorApprovedEvent(product, true);
  }


  @Test
   void deleteOriginalImagesForProductAndItemsTest() throws  Exception {
    deleteProductImages.get(0).setLocationPath(FILE_NAME_PATH1);
    deleteProductItemImages.get(0).setLocationPath(FILE_NAME_PATH2);
    Set<String> set = new HashSet<>();
    Product product = new Product();
    createFileAndSetToProductAndItem(product);
    set.add(FILE_NAME_PATH1);
    set.add(FILE_NAME_PATH2);
    productImages.forEach(productImage -> productImage.setOriginalImage(true));
    Mockito.when(productImageRepository.findByProductId(any())).thenReturn(productImages);
    instance.deleteOriginalImagesForProductAndItems(product);
    Mockito.verify(productImageRepository).findByProductId(Mockito.any());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.any());
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
  void deleteOriginalImagesForProductAndItemsTest_enabledEventBased() throws Exception {
    ReflectionTestUtils.setField(instance, "deleteImagesEventBasedEnabled", true);
    deleteProductImages.get(0).setLocationPath(FILE_NAME_PATH1);
    deleteProductItemImages.get(0).setLocationPath(FILE_NAME_PATH2);
    Set<String> set = new HashSet<>();
    Product product = new Product();
    createFileAndSetToProductAndItem(product);
    set.add(FILE_NAME_PATH1);
    set.add(FILE_NAME_PATH2);
    productImages.forEach(productImage -> productImage.setOriginalImage(true));
    Mockito.when(productImageRepository.findByProductId(any())).thenReturn(productImages);
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteOriginalImageEvent()).thenReturn(TOPIC);
    instance.deleteOriginalImagesForProductAndItems(product);
    Mockito.verify(productImageRepository).findByProductId(Mockito.any());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.any());
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteOriginalImageEvent();
    Mockito.verify(kafkaProducer).send(eq(TOPIC), eq(null), Mockito.any());
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  @Test
   void deleteOriginalImagesForProductAndItemsExceptionTest() throws Exception {
    deleteProductImages.get(0).setLocationPath(FILE_NAME_PATH1);
    deleteProductItemImages.get(0).setLocationPath(FILE_NAME_PATH2);
    Set<String> set = new HashSet<>();
    Product product = new Product();
    createFileAndSetToProductAndItem(product);
    set.add(FILE_NAME_PATH1);
    set.add(FILE_NAME_PATH2);
    productImages.forEach(productImage -> productImage.setOriginalImage(true));
    Mockito.when(productImageRepository.findByProductId(any())).thenReturn(productImages);
    Mockito.doThrow(ApplicationRuntimeException.class).when(fileStorageService).deleteOriginalImages(anyString());
    instance.deleteOriginalImagesForProductAndItems(product);
    Mockito.verify(productImageRepository).findByProductId(Mockito.any());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.any());
    Mockito.verify(fileStorageService).deleteOriginalImages(anyString());
    FileUtils.deleteDirectory(new File(BASE_FOLDER));
  }

  private void createFileAndSetToProductAndItem(Product product) throws IOException {
    File mainDirectory = new File(BASE_FOLDER);
    mainDirectory.mkdir();
    File directory1 = new File(mainDirectory, "1");
    directory1.mkdir();
    File directory2 = new File(directory1, PRODUCT_CODE2);
    directory2.mkdir();
    file1 = new File(directory2 + File.separator + FILE_NAME1);
    file2 = new File(directory2 + File.separator + FILE_NAME2);
    file1.createNewFile();
    file2.createNewFile();
    ProductImage productImage = new ProductImage();
    productImage.setOriginalImage(Boolean.TRUE);
    ProductImage productImage1 = new ProductImage();
    productImage1.setOriginalImage(Boolean.FALSE);
    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(null);
    productImage.setLocationPath(FILE_NAME_PATH1);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(Boolean.TRUE);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setOriginalImage(Boolean.FALSE);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setOriginalImage(null);
    productItemImage.setLocationPath(FILE_NAME_PATH2);
    product.setProductImages(Arrays.asList(productImage, productImage1, productImage2));
    product.setProductItems(List.of(new ProductItem()));
    product.getProductItems().get(0)
        .setProductItemImages(Arrays.asList(productItemImage, productItemImage1, productItemImage2));
  }

  @Test
   void updateImageQcResponseByProductCodeTest() throws Exception {
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(), PRODUCT_CODE)).thenReturn(Constants.ACTIVE);
    instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeAutoNeedRevisionTest() throws Exception {
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(true);
    autoNeedRevisionAndForceReviewResponse.setPredictionTypeSet(Collections.singleton(PREDICTION_TYPE_1));
    imageQcProcessedResponseDomainEvent
        .setAutoNeedRevisionAndForceReviewResponse(autoNeedRevisionAndForceReviewResponse);
    imageQcProcessedResponseDomainEvent.setTextViolations(PREDICTION_TYPE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT, PRODUCT_CODE,
            new AutoNeedRevisionDomainEvent(product.getStoreId(), product.getProductCode(), new ArrayList<>(
                imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse()
                    .getPredictionTypeSet())));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertTrue(productArgumentCaptor.getValue().isAutoNeedRevision());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertEquals(PREDICTION_TYPE_1,
        productArgumentCaptor.getValue().getTextViolations());
  }

  @Test
   void updateImageQcResponseByProductCodeAutoNeedRevisionRevisedTest() throws Exception {
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(true);
    autoNeedRevisionAndForceReviewResponse.setPredictionTypeSet(Collections.singleton(PREDICTION_TYPE_1));
    product.setRevised(true);
    imageQcProcessedResponseDomainEvent
        .setAutoNeedRevisionAndForceReviewResponse(autoNeedRevisionAndForceReviewResponse);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    imageQcProcessedResponseDomainEvent.setPredictedBrand(BRAND_NAME);
    instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT, PRODUCT_CODE,
            new AutoNeedRevisionDomainEvent(product.getStoreId(), product.getProductCode(), new ArrayList<>(
                imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse()
                    .getPredictionTypeSet())));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertTrue(productArgumentCaptor.getValue().isAutoNeedRevision());
    Assertions.assertEquals(BRAND_NAME, productArgumentCaptor.getValue().getPredictedBrand());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeAutoNeedRevisionTestWithAutoApprovalEligible() throws Exception {
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(true);
    autoNeedRevisionAndForceReviewResponse.setPredictionTypeSet(Collections.singleton(PREDICTION_TYPE_1));
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    imageQcProcessedResponseDomainEvent
        .setAutoNeedRevisionAndForceReviewResponse(autoNeedRevisionAndForceReviewResponse);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_AUTO_NEED_REVISION_EVENT, PRODUCT_CODE,
            new AutoNeedRevisionDomainEvent(product.getStoreId(), product.getProductCode(), new ArrayList<>(
                imageQcProcessedResponseDomainEvent.getAutoNeedRevisionAndForceReviewResponse()
                    .getPredictionTypeSet())));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertTrue(productArgumentCaptor.getValue().isAutoNeedRevision());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeEmptyImageViolationTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setImageViolations(StringUtils.EMPTY);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertNull(productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(true);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.ACTIVE);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishEditedVendorApprovedEvent(Mockito.any(Product.class)))
        .thenReturn(new PDTEditedProductVendorApprovedEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();

    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestIsRevisedImangeApprovedAutoApprovalTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(true);
    product.setEdited(true);
    product.setBrandApprovalStatus("APPROVED");
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.ACTIVE);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishEditedVendorApprovedEvent(Mockito.any(Product.class)))
        .thenReturn(new PDTEditedProductVendorApprovedEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeImageApprovedAutoApprovalTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(true);
    product.setEdited(true);
    product.setBrandApprovalStatus("APPROVED");
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.ACTIVE);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishEditedVendorApprovedEvent(Mockito.any(Product.class)))
        .thenReturn(new PDTEditedProductVendorApprovedEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalInReviewTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(true);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishEditedVendorApprovedEvent(Mockito.any(Product.class)))
        .thenReturn(new PDTEditedProductVendorApprovedEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.ACTIVE);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalNewProductTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishAutoApprovalEvent(Mockito.any(PDTAutoApprovalEventModel.class)))
        .thenReturn(new PDTAutoApprovalEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.ACTIVE);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertTrue(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalNewProductTestWithProductNotActive() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishAutoApprovalEvent(Mockito.any(PDTAutoApprovalEventModel.class)))
        .thenReturn(new PDTAutoApprovalEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(Constants.NOT_ACTIVE_ERROR_MESSAGE);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(new ProductReviewer());

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.any(), Mockito.any(), Mockito.anyMap());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
    Mockito.verify(productReviewerService).saveAndFlush(productReviewerArgumentCaptor.capture());
    Assertions.assertEquals(Constants.NOT_ACTIVE_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertFalse(productReviewerArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalNewProductTestWithProductNotActiveStateEmpty()
      throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productUtils).removeOriginalImagesFromProduct(Mockito.any(Product.class));
    Mockito.when(approvedProductPublisherService.publishAutoApprovalEvent(Mockito.any(PDTAutoApprovalEventModel.class)))
        .thenReturn(new PDTAutoApprovalEventModel());
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productServiceRepository.getProductStatus(product.getStoreId(),
        imageQcProcessedResponseDomainEvent.getProductCode())).thenReturn(StringUtils.EMPTY);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(null);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productServiceRepository)
        .getProductStatus(product.getStoreId(), imageQcProcessedResponseDomainEvent.getProductCode());
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productAutoApprovalService)
        .addProductsToAutoApprovalTable(Mockito.any(), Mockito.anyList(), Mockito.anyMap());
    Mockito.verify(productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertEquals(Constants.NOT_ACTIVE_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalBrandInReviewTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("DRAFT");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(Constants.BRAND_APPROVAL_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalPreliveTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(false);
    product.setBrandApprovalStatus("DRAFT");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(Constants.PRODUCT_PRELIVE_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalProductContentAssignedTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setStoreId(STORE_ID);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(Constants.ALREADY_ASSIGNED_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalProductImageAssignedTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setStoreId(STORE_ID);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.IN_REVIEW);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(Constants.ALREADY_ASSIGNED_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalInvalidStateTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    product.setPostLive(true);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("APPROVED");
    product.setState(WorkflowState.UNASSIGNED);

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(Constants.PRODUCT_STATE_ERROR_MESSAGE,
        publishAndSavedProductAndHistoryModel.getInternalHistoryEventModel().getNotes());
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateImageQcResponseByProductCodeTestAutoApprovalNAApprivalTypeTest() throws Exception {
    imageQcProcessedResponseDomainEvent.setAutoApprovalType(AutoApprovalType.NA);
    product.setPostLive(false);
    product.setRevised(false);
    product.setEdited(false);
    product.setBrandApprovalStatus("DRAFT");

    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);

    PublishAndSavedProductAndHistoryModel publishAndSavedProductAndHistoryModel =
        instance.updateImageQcResponseByProductCode(imageQcProcessedResponseDomainEvent, product);
    boolean publishEvent = publishAndSavedProductAndHistoryModel.getDoPublish();
    Mockito.verify(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequestArgumentCaptor.capture(), Mockito.eq(false),
            Mockito.eq(true));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertFalse(publishEvent);
    Assertions.assertEquals(PRODUCT_CODE, productArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_VIOLATION, productArgumentCaptor.getValue().getImageViolations());
    Assertions.assertEquals(10, productArgumentCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(IMAGE_QC_RESPONSE,
        productImageQcFeedbackRequestArgumentCaptor.getValue().getSystemFeedback());
  }

  @Test
   void updateBrandApprovalStatusTest() {
    List<Object[]> objects = new ArrayList<>();
    objects.add(new Object[] {PRODUCT_ID, PRODUCT_ITEM_ID1, PRODUCT_CODE});

    Mockito.when(productRepository.findIdByBrandCodeAndMarkForDeleteFalse(
        brandApprovedOrRejectedDomainEventModel.getBrandRequestCode())).thenReturn(objects);
    Mockito.doNothing().when(productRepository)
        .updateBrandDetail(brandApprovedOrRejectedDomainEventModel.getBrandRequestCode(),
            brandApprovedOrRejectedDomainEventModel.getBrandName(),
            brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(), List.of(PRODUCT_ID));
    Mockito.doNothing().when(productAttributeRespository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ID));
    Mockito.doNothing().when(productItemAttributeRepository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ITEM_ID1));

    List<String> result = instance.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);

    Mockito.verify(productRepository)
        .findIdByBrandCodeAndMarkForDeleteFalse(brandApprovedOrRejectedDomainEventModel.getBrandRequestCode());
    Mockito.verify(productRepository).updateBrandDetail(brandApprovedOrRejectedDomainEventModel.getBrandRequestCode(),
        brandApprovedOrRejectedDomainEventModel.getBrandName(),
        brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(), List.of(PRODUCT_ID));
    Mockito.verify(productAttributeRespository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ID));
    Mockito.verify(productItemAttributeRepository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ITEM_ID1));

    Assertions.assertEquals(PRODUCT_CODE, result.get(0));
  }

  @Test
   void updateBrandApprovalStatusBrandCodeTest() {
    brandApprovedOrRejectedDomainEventModel.setBrandCode(BRAND_CODE);
    List<Object[]> objects = new ArrayList<>();
    objects.add(new Object[] {PRODUCT_ID, PRODUCT_ITEM_ID1, PRODUCT_CODE});

    Mockito.when(productRepository.findIdByBrandCodeAndMarkForDeleteFalse(
        brandApprovedOrRejectedDomainEventModel.getBrandRequestCode())).thenReturn(objects);
    Mockito.doNothing().when(productRepository)
        .updateBrandDetail(brandApprovedOrRejectedDomainEventModel.getBrandCode(),
            brandApprovedOrRejectedDomainEventModel.getBrandName(),
            brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(), List.of(PRODUCT_ID));
    Mockito.doNothing().when(productAttributeRespository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ID));
    Mockito.doNothing().when(productItemAttributeRepository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ITEM_ID1));

    List<String> result = instance.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);

    Mockito.verify(productRepository)
        .findIdByBrandCodeAndMarkForDeleteFalse(brandApprovedOrRejectedDomainEventModel.getBrandRequestCode());
    Mockito.verify(productRepository).updateBrandDetail(brandApprovedOrRejectedDomainEventModel.getBrandCode(),
        brandApprovedOrRejectedDomainEventModel.getBrandName(),
        brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(), List.of(PRODUCT_ID));
    Mockito.verify(productAttributeRespository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ID));
    Mockito.verify(productItemAttributeRepository)
        .updateValueByNameAndProductIds(Constants.BRAND, brandApprovedOrRejectedDomainEventModel.getBrandName(),
            List.of(PRODUCT_ITEM_ID1));

    Assertions.assertEquals(PRODUCT_CODE, result.get(0));
  }

  @Test
   void updateBrandApprovalStatusNoProductIdOrItemIdTest() {
    Mockito.when(productRepository.findIdByBrandCodeAndMarkForDeleteFalse(
        brandApprovedOrRejectedDomainEventModel.getBrandRequestCode())).thenReturn(new ArrayList<>());

    List<String> result = instance.updateBrandApprovalStatus(brandApprovedOrRejectedDomainEventModel);

    Mockito.verify(productRepository)
        .findIdByBrandCodeAndMarkForDeleteFalse(brandApprovedOrRejectedDomainEventModel.getBrandRequestCode());

    Assertions.assertTrue(result.isEmpty());
  }

  @Test
   void findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrueTest() {
    Date date = new Date();
    Mockito.when(
        productRepository.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(), eq(pageable)))
        .thenReturn(Mockito.any());
    instance.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(eq(date), pageable);
    Mockito.verify(productRepository)
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(), eq(pageable));
  }

  @Test
   void findByStoreIdAndMarkForDeleteFalseTest() {
    product.setVendorId(VENDOR_ID);
    productPage = new PageImpl<>(List.of(product), pageable, 1);
    Mockito.when(this.productRepository
        .findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable))
        .thenReturn(productPage);
    Page<Product> result = this.instance.findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable);
    Mockito.verify(this.productRepository).findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
        STORE_ID, pageable);
    Assertions.assertEquals(PRODUCT_CODE, result.getContent().get(0).getProductCode());
  }

  @Test
   void findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAscTest() {
    product.setVendorId(VENDOR_ID);
    productPage = new PageImpl<>(List.of(product), pageable, 1);
    Mockito.when(this.productRepository
                    .findByStoreIdAndUpdatedDateBetween(STORE_ID, DEFAULT_START_DATE, DEFAULT_END_DATE, pageable))
            .thenReturn(productPage);
    Page<Product> result = this.instance.findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(STORE_ID,
            DEFAULT_START_DATE, DEFAULT_END_DATE, pageable);
    Mockito.verify(this.productRepository).findByStoreIdAndUpdatedDateBetween(
            STORE_ID, DEFAULT_START_DATE, DEFAULT_END_DATE, pageable);
    Assertions.assertEquals(PRODUCT_CODE, result.getContent().get(0).getProductCode());
  }

  @Test
   void getReviewConfigProductCountByVendorAndConfigTest() throws Exception {
    Mockito.when(
        this.solrVendorCollectionService.getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE))
        .thenReturn(new HashMap<>());
    Map<String, Object> productStatusByVendor =
        instance.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
    Mockito.verify(this.solrVendorCollectionService)
        .getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
  }

  @Test
   void getReviewConfigProductCountByVendorAndConfigExceptionTest() throws Exception {
    Mockito.when(
        this.solrVendorCollectionService.getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE))
        .thenThrow(SolrException.class);
    Mockito.when(vendorRepository.getVendorIdByVendorCode(VENDOR_CODE)).thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, false));
    } finally {
      Mockito.verify(this.solrVendorCollectionService)
          .getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
    }
  }

  @Test
   void updateApprovedProductDataImageTest() {
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(oldProduct, newProduct)).thenReturn(oldProduct);
    instance.updateApprovedProductData(oldProduct, newProduct);
    Mockito.verify(productUtils).replaceProductImageAndProductItemImages(oldProduct, newProduct);
    Mockito.verify(productUtils).regenerateProductImageDetails(oldProduct);
    Mockito.verify(productUtils).replaceProductDetails(oldProduct, newProduct, true);
  }

  @Test
   void republishEditedProductApprovalEvent() throws Exception {
    product.setEdited(true);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doReturn(null).when(approvedProductPublisherService).publishEditedVendorApprovedEvent(Mockito.any(Product.class));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(productArgumentCaptor.capture());
  }

  @Test
   void republishEditedProductApprovalEventPassedTest() throws Exception {
    product.setEdited(true);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doReturn(null).when(approvedProductPublisherService).publishEditedVendorApprovedEvent(Mockito.any(Product.class));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(productArgumentCaptor.capture());
  }

  @Test
   void republishEditedProductApprovalEventNotPassedTest() {
    product.setEdited(true);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doReturn(null).when(approvedProductPublisherService).publishEditedVendorApprovedEvent(Mockito.any(Product.class));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(productArgumentCaptor.capture());
  }

  @Test
   void republishEditedProductApprovalEventNotedited() {
    product.setEdited(false);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doReturn(null).when(approvedProductPublisherService).publishEditedVendorApprovedEvent(Mockito.any(Product.class));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
   void republishEditedProductApprovalEventNullProduct() {
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(null);
    Mockito.doReturn(null).when(approvedProductPublisherService).publishEditedVendorApprovedEvent(Mockito.any(Product.class));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
   void updateRevisedProductDataTest() throws IOException {
    newProduct.setState(WorkflowState.IN_REVIEW);
    newProduct.setReviewType(ReviewType.CONTENT);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    newProduct.setRestrictedKeywordsDetected(RESTRICTED_KEYWORDS_FIELD);
    instance.updateRevisedProductData(oldProduct, newProduct, modifiedFields);
    Mockito.verify(productUtils).replaceProductDetails(oldProduct, newProduct, false);
    Mockito.verify(this.productRepository).save(productArgumentCaptor.capture());
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Assertions.assertEquals(RESTRICTED_KEYWORDS_FIELD,
        productArgumentCaptor.getValue().getRestrictedKeywordsDetected());
  }

  @Test
   void updateRevisedProductDataImageTest() throws IOException {
    newProduct.setState(WorkflowState.IN_REVIEW);
    newProduct.setReviewType(ReviewType.IMAGE);
    oldProduct.setProductNotes(NOTES);
    Mockito.when(mapper.readValue(oldProduct.getProductNotes(), ProductNotesResponse.class))
        .thenReturn(new ProductNotesResponse());
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(oldProduct, newProduct)).thenReturn(oldProduct);
    Product existingProduct = instance.updateRevisedProductData(oldProduct, newProduct, modifiedFields);
    Mockito.verify(productUtils).replaceProductImageAndProductItemImages(oldProduct, newProduct);
    Mockito.verify(productUtils).regenerateProductImageDetails(oldProduct);
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(mapper).readValue(oldProduct.getProductNotes(), ProductNotesResponse.class);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateRevisedProductDataImageStateContentUpdatedTest() throws IOException {
    newProduct.setState(WorkflowState.IN_REVIEW);
    newProduct.setReviewType(ReviewType.IMAGE);
    modifiedFields.add("content");
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(oldProduct, newProduct)).thenReturn(oldProduct);
    instance.updateRevisedProductData(oldProduct, newProduct, modifiedFields);
    Mockito.verify(productUtils).replaceProductImageAndProductItemImages(oldProduct, newProduct);
    Mockito.verify(productUtils).regenerateProductImageDetails(oldProduct);
    Mockito.verify(productUtils).replaceProductDetails(oldProduct, newProduct, false);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void updateRevisedProductDataContentStateImageUpdatedTest() throws IOException {
    newProduct.setState(WorkflowState.IN_REVIEW);
    newProduct.setReviewType(ReviewType.CONTENT);
    modifiedFields.add("image");
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(oldProduct, newProduct)).thenReturn(oldProduct);
    instance.updateRevisedProductData(oldProduct, newProduct, modifiedFields);
    Mockito.verify(productUtils).replaceProductImageAndProductItemImages(oldProduct, newProduct);
    Mockito.verify(productUtils).regenerateProductImageDetails(oldProduct);
    Mockito.verify(productUtils).replaceProductDetails(oldProduct, newProduct, false);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void updateRevisedProductDataInReviewTest() throws IOException {
    newProduct.setState(WorkflowState.IN_REVIEW);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.when(productUtils.replaceProductImageAndProductItemImages(oldProduct, newProduct)).thenReturn(oldProduct);
    instance.updateRevisedProductData(oldProduct, newProduct, modifiedFields);
    Mockito.verify(productUtils).replaceProductImageAndProductItemImages(oldProduct, newProduct);
    Mockito.verify(productUtils).regenerateProductImageDetails(oldProduct);
    Mockito.verify(productUtils).replaceProductDetails(oldProduct, newProduct, false);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void republishRevisedProductApprovalEvent() {
    product.setRevised(true);
    product.setEdited(true);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doReturn(null).when(approvedProductPublisherService)
        .publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(Boolean.FALSE));
    instance.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(approvedProductPublisherService)
        .publishRevisedVendorApprovedEvent(Mockito.any(Product.class), Mockito.eq(Boolean.FALSE));
  }

  @Test
   void autoApprovePendingProductsEligibleNullEditStateApprovedContentAssignedTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleNullEditStateImageAssignedTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleNullEditStateTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNull(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService)
        .updateProductAutoApprovalDetails(productAutoApprovalArgumentCaptor.capture());
    Assertions.assertEquals(ErrorMessages.AUTO_APPROVAL_PRODUCT_ASSIGNMENT,
        productAutoApprovalArgumentCaptor.getValue().getReasonOfFailure());
  }

  @Test
   void autoApprovePendingProductsEligibleProductReviewerNullTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(null);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNullDateNull(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.AUTO_APPROVED);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNullDateNotNull(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE_ZERO);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNullDateNullImage(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.AUTO_APPROVED);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any());
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNullDateNotNullImage(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE_ZERO);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsEligibleTestApproverAssigneeNullImage(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setEdited(true);
    product.setReviewType(ReviewType.IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
  }

  @Test
   void autoApprovePendingProductsEligibleTestImageApproverAssigneeNull(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.IN_REVIEW);
    product.setEdited(true);
    product.setReviewType(ReviewType.IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setApproverAssignee(Constants.INCORRECT_ASSIGNEE_MESSAGE);
    productReviewer.setAssignedDate(null);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
  }

  @Test
   void autoApprovePendingProductsEligibleTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleRevisionTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsEligibleOnlyCategoryCodeUpdateTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.STATUS);
    productAutoApprovalList.get(0).setCategoryCode(CATEGORY_CODE);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsStateApprovedTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsBrandUnApproveTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(UNASSIGN);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService)
        .updateProductAutoApprovalDetails(productAutoApprovalArgumentCaptor.capture());
    Assertions.assertEquals(ErrorMessages.AUTO_APPROVAL_PRODUCT_BRAND_NOT_ACTIVE,
        productAutoApprovalArgumentCaptor.getValue().getReasonOfFailure());
  }

  @Test
   void autoApprovePendingProductsStateApprovedBrandUnApproveTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsRevisedTrueTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService)
        .updateProductAutoApprovalDetails(productAutoApprovalArgumentCaptor.capture());
    Assertions.assertEquals(
        ErrorMessages.AUTO_APPROVAL_PRODUCT_NOT_IN_REVIEW + WorkflowState.UNASSIGNED,
        productAutoApprovalArgumentCaptor.getValue().getReasonOfFailure());
  }

  @Test
   void autoApprovePendingProductsStateApprovedIsRevisedTrueTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsBrandUnApproveIsRevisedTrueTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsStateApprovedBrandUnApproveIsRevisedTrueTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService)
        .updateProductAutoApprovalDetails(productAutoApprovalArgumentCaptor.capture());
    Assertions.assertEquals(ErrorMessages.AUTO_APPROVAL_PRODUCT_NOT_POST_LIVE,
        productAutoApprovalArgumentCaptor.getValue().getReasonOfFailure());
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseStateApprovedTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseBrandUnApproveTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseStateApprovedBrandUnApproveTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseIsRevisedTrueTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseStateApprovedIsRevisedTrueTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseBrandUnApproveIsRevisedTrueTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.UNASSIGNED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsPostLiveFalseStateApprovedBrandUnApproveIsRevisedTrueTest(){
    product.setPostLive(Boolean.FALSE);
    product.setRevised(Boolean.TRUE);
    product.setBrandApprovalStatus(UNASSIGN);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAndImageAutoApproved(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAndImageAutoApprovedEdited(){
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAndImageAutoApprovedEditedContent(){
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAndImageAutoApprovedEditedImage(){
    product.setEdited(true);
    product.setReviewType(ReviewType.IMAGE);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAndImageAssignedLongBackAutoApproved(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
  }

  @Test
   void autoApprovePendingProductsIsContentAssignedNowNotEligibleForAutoApproval(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsImageAssignedNowNotEligibleForAutoApproval(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsProductNotInInReviewNotEligibleForAutoApproval(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setState(WorkflowState.PASSED);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsProductNotInInReviewContentAndImageNotEligibleForAutoApproval(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setState(WorkflowState.PASSED);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEditedProductImageNotEligibleForAutoApproval(){
    product.setEdited(true);
    product.setReviewType(ReviewType.IMAGE);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setState(WorkflowState.PASSED);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEditedProductContentAndImageEditedNotEligibleForAutoApproval(){
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setState(WorkflowState.PASSED);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEditedProductContentEditedNotEligibleForAutoApproval(){
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setState(WorkflowState.PASSED);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsIsImageAssignedNullNotEligibleForAutoApproval(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleNullProductTest(){
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        List.of(product1));
    instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApprovePendingProductsEligibleExceptionTest(){
    product.setPostLive(Boolean.TRUE);
    product.setRevised(Boolean.FALSE);
    product.setBrandApprovalStatus(Constants.APPROVED);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
      instance.autoApprovePendingProducts(STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
      Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(kafkaProducer).send(eq(DomainEventName.PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME),
        eq(PRODUCT_CODE), any(ProductAutoApprovalEventModel.class));
      Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetails(any(ProductAutoApproval.class));
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckNotEligibleTest() throws Exception {
    instance.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.FALSE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(product.getStoreId(),
        PRODUCT_CODE, AutoApprovalStatus.NA, true);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckFailedTest() throws Exception {
    this.product.setPostLive(false);
    instance.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productAutoApprovalService).updateProductAutoApprovalDetailsByProductCode(product.getStoreId(),
        PRODUCT_CODE, AutoApprovalStatus.NA, true);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckSuccessTest() throws Exception {
    this.product.setPostLive(true);
    this.product.setRevised(false);
    this.product.setBrandApprovalStatus(Constants.APPROVED);
    this.product.setEdited(true);
    productReviewer.setApproverAssignee(null);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    instance.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(productArgumentCaptor.capture());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Assertions.assertEquals(productArgumentCaptor.getValue().getState(), WorkflowState.PASSED);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckSuccessContentEditTest() throws Exception {
    this.product.setPostLive(true);
    this.product.setRevised(false);
    this.product.setBrandApprovalStatus(Constants.APPROVED);
    this.product.setEdited(true);
    this.product.setReviewType(ReviewType.CONTENT);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(productArgumentCaptor.capture());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(any(ProductReviewer.class));
    Assertions.assertEquals(productArgumentCaptor.getValue().getState(), WorkflowState.PASSED);
  }

  @Test
   void autoApproveOfPendingProductsAfterEligibilityCheckSuccessImageEditTest() throws Exception {
    this.product.setPostLive(true);
    this.product.setRevised(false);
    this.product.setBrandApprovalStatus(Constants.APPROVED);
    this.product.setEdited(true);
    this.product.setReviewType(ReviewType.IMAGE);
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString()))
        .thenReturn(new ProductReviewer());
    instance.autoApproveOfPendingProductsAfterEligibilityCheck(product, Boolean.TRUE, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
    Mockito.verify(productUtils).removeOriginalImagesFromProduct(productArgumentCaptor.capture());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService).save(any(ProductReviewer.class));
    Assertions.assertEquals(productArgumentCaptor.getValue().getState(), WorkflowState.PASSED);

  }

  @Test
   void updateProductToAutoNeedRevisionTest() {
    productReviewer.setApproverAssignee(null);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productDistributionTaskService.findByProductId(product.getId())).thenReturn(productDistributionTask);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService, Mockito.times(1)).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
    Assertions.assertTrue(productArgumentCaptor.getValue().isAutoNeedRevision());
  }

  @Test
   void updateProductToAutoNeedRevisionContentAssignedTest() {
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, true));
    } finally {
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    }
  }

  @Test
   void updateProductToAutoNeedRevisionContentAssignedisImageEventFalseTest() {
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(product);
    instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.FALSE);
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }
  @Test
   void updateProductToAutoNeedRevisionContentAssignedisImageEventFalse_Test() {
    productReviewer.setApproverAssignee(null);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(product);
    instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.FALSE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void updateProductToAutoNeedRevisionProductReviewerNullTest() {
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, true));
    } finally {
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    }
  }

  @Test
   void updateProductToAutoNeedRevisionImageAssignedTest() {
    productReviewer.setApproverAssignee(TEST_ASSIGNEE);
    productReviewer.setAssignedDate(new Date());
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, true));
    } finally {
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    }
  }

  @Test
   void updateProductToAutoNeedRevisionEditedImageTest() {
    this.product.setEdited(true);
    this.product.setReviewType(ReviewType.IMAGE);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productDistributionTaskService.findByProductId(product.getId())).thenReturn(productDistributionTask);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productReviewerService, Mockito.times(1)).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
  }

  @Test
   void updateProductToAutoNeedRevisionEditedContentTest() {
    this.product.setEdited(true);
    this.product.setReviewType(ReviewType.CONTENT);
    this.productReviewer.setApproverAssignee(null);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productDistributionTaskService.findByProductId(product.getId())).thenReturn(productDistributionTask);
    Mockito.when(productRepository.saveAndFlush(Mockito.any(Product.class))).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, Boolean.TRUE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productReviewerService, Mockito.times(1)).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
  }

  @Test
   void updateProductTaskAndHistoryTest() {
    Mockito.when(productDistributionTaskService.findByProductId(product.getId())).thenReturn(productDistributionTask);
    instance.updateProductTaskAndHistory(product, autoNeedRevisionRequest);
    Mockito.verify(productDistributionTaskService).findByProductId(product.getId());
    Mockito.verify(productDistributionTaskService).updateState(productDistributionTask, product.getState());
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(product.getId());
    Mockito.verify(taskHistoryRepository).save(any(TaskHistory.class));
  }

  @Test
   void updateProductToAutoNeedRevisionNullProductTest() {
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductToAutoNeedRevision(STORE_ID, autoNeedRevisionRequest, true));
    } finally {
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    }
  }

  @Test
   void autoNeedReviseForPendingProductsTest(){
    Mockito.when(applicationContext.getBean(ProductService.class)).thenReturn(productService);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productServiceRepository.retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class)))
        .thenReturn(retryAutoNeedRevisionResponse);
    productReviewer.setApproverAssignee(null);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productServiceRepository).retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    Mockito.verify(productActionRetryService).updateProductActionRetryDetails(any(ProductActionRetry.class));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
  }

  @Test
   void autoNeedReviseForPendingProductsProductNotActiveTest(){
    productReviewer.setApproverAssignee(null);
    this.retryAutoNeedRevisionResponse.setProductActive(false);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productServiceRepository.retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class)))
        .thenReturn(retryAutoNeedRevisionResponse);
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productServiceRepository).retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    Mockito.verify(productActionRetryService).updateProductActionRetryDetails(any(ProductActionRetry.class));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
  }

  @Test
   void autoNeedReviseForPendingProductsProductAssignedTest(){
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productActionRetryService).updateProductActionRetryDetails(any(ProductActionRetry.class));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
  }

  @Test
   void autoNeedReviseForPendingProductsProductNullTest(){
    product.setProductCode(PRODUCT_CODE1);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productActionRetryService).updateProductActionRetryDetails(any(ProductActionRetry.class));
  }

  @Test
   void autoNeedReviseForPendingProductsExceptionTest(){
    productReviewer.setApproverAssignee(null);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productActionRetryService.getProductActionRetryByProductCodeAndAction(STORE_ID,
        PRODUCT_CODE, ACTIVE_STATE)).thenReturn(productActionRetryList.get(0));
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    Mockito.doThrow(RuntimeException.class).when(productServiceRepository)
        .retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productServiceRepository).retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    Mockito.verify(productActionRetryService).updateProductActionRetryDetails(any(ProductActionRetry.class));
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
    Mockito.verify(productActionRetryService).getProductActionRetryByProductCodeAndAction(STORE_ID, PRODUCT_CODE, ACTIVE_STATE);
  }

  @Test
   void autoNeedReviseForPendingProductsExceptionNullTest(){
    productReviewer.setApproverAssignee(null);
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList())).thenReturn(
        Collections.singletonList(product));
    Mockito.when(productActionRetryService.getProductActionRetryByProductCodeAndAction(STORE_ID,
        PRODUCT_CODE, ACTIVE_STATE)).thenReturn(null);
    Mockito.when(productReviewerService.findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode())).thenReturn(productReviewer);
    Mockito.doThrow(RuntimeException.class).when(productServiceRepository)
        .retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    instance.autoNeedReviseForPendingProducts(STORE_ID, productActionRetryList);
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(anyList());
    Mockito.verify(productServiceRepository).retryAutoNeedRevision(eq(STORE_ID), any(RetryNeedRevisionRequest.class));
    Mockito.verify(productActionRetryService).getProductActionRetryByProductCodeAndAction(STORE_ID, PRODUCT_CODE, ACTIVE_STATE);
    Mockito.verify(productReviewerService).findProductReviewerByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        product.getProductCode());
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelTest() throws IOException {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().productCode(PRODUCT_CODE).build();
    Product product = new Product();
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.when(productRepository.save(product)).thenReturn(product);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).save(product);
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelDimensionRefreshTest() throws IOException {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().productCode(PRODUCT_CODE).shippingWeight(VALUE_1).build();
    Product product = new Product();
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.when(productRepository.save(product)).thenReturn(product);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).save(product);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelDimensionChangeTest() throws IOException {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().productCode(PRODUCT_CODE).build();
    Product product = new Product();
    product.setShippingWeight(VALUE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.when(productRepository.save(product)).thenReturn(product);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Mockito.verify(productRepository).save(product);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelWeightChangeTest() throws IOException {
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product2);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.when(productRepository.save(product2)).thenReturn(product2);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).save(product2);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelHeightChangeTest() throws IOException {
    pdtDimensionRefreshEventModel2.setWeight(VALUE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product2);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.when(productRepository.save(product2)).thenReturn(product2);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).save(product2);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelWidthChangeTest() throws IOException {
    pdtDimensionRefreshEventModel2.setWeight(VALUE_1);
    pdtDimensionRefreshEventModel2.setHeight(VALUE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product2);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.when(productRepository.save(product2)).thenReturn(product2);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).save(product2);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelLengthChangeTest() throws IOException {
    pdtDimensionRefreshEventModel2.setWeight(VALUE_1);
    pdtDimensionRefreshEventModel2.setHeight(VALUE_1);
    pdtDimensionRefreshEventModel2.setWidth(VALUE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product2);
    Mockito.when(mapper.writeValueAsString(any(ProductNotesResponse.class))).thenReturn(NOTES);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.when(productRepository.save(product2)).thenReturn(product2);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).save(product2);
    Mockito.verify(mapper).writeValueAsString(any(ProductNotesResponse.class));
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelNoChangeTest() throws IOException {
    pdtDimensionRefreshEventModel2.setWeight(VALUE_1);
    pdtDimensionRefreshEventModel2.setHeight(VALUE_1);
    pdtDimensionRefreshEventModel2.setWidth(VALUE_1);
    pdtDimensionRefreshEventModel2.setLength(VALUE_1);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product2);
    Mockito.doNothing().when(productUtils)
        .setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.when(productRepository.save(product2)).thenReturn(product2);
    instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productUtils).setProductDimensionsAndProductTypeAndDgLevel(product2, pdtDimensionRefreshEventModel2);
    Mockito.verify(productRepository).save(product2);
  }

  @Test
   void updateProductDimensionsAndProductTypeAndDgLevelProductNotFoundTest()
      throws IOException {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().productCode(PRODUCT_CODE).build();
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductDimensionsAndProductTypeAndDgLevel(pdtDimensionRefreshEventModel));
    } finally {
      Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    }
  }

  @Test
   void autoApproveProductTest() throws Exception {
    product.setPostLive(false);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(anyString())).thenReturn(productReviewer);
    instance.autoApproveProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productRepository).saveAndFlush(product);
    Mockito.verify(solrVendorCollectionService).updateImageQcResponseToSolr(product, productReviewer);
    Mockito.verify(approvedProductPublisherService).publishAutoApprovalEvent(Mockito.any());
  }

  @Test
   void getProductsByProductCodesTest() throws Exception {
    Mockito.when(productRepository.findByProductCodeInAndMarkForDeleteFalse(anyList()))
        .thenReturn(Collections.singletonList(product));
    List<Product> response = instance.getProductsByProductCodes(List.of(PRODUCT_CODE));
    Mockito.verify(productRepository).findByProductCodeInAndMarkForDeleteFalse(
        List.of(PRODUCT_CODE));
  }

  @Test
   void autoApproveProductTestNotesEmpty() throws Exception {
    product.setPostLive(true);
    product.setBrandApprovalStatus(Constants.APPROVED);
    product.setRevised(true);
    List<ProductItem> productItemList = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setOriginalImage(true);
    productItemImage.setLocationPath(LOCATION_PATH);
    productItemImageList.add(productItemImage);
    productItem.setProductItemImages(productItemImageList);
    productItemList.add(productItem);
    product.setProductItems(productItemList);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(productServiceRepository.getProductStatus(Mockito.anyString(), Mockito.anyString())).thenReturn(NOT_ACTIVE);
    Mockito.when(productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(productReviewer);
    instance.autoApproveProduct(PRODUCT_CODE);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productServiceRepository).getProductStatus(Mockito.any(), Mockito.anyString());
    Mockito.verify(productReviewerService, Mockito.times(2)).findProductReviewerByProductCode(Mockito.anyString());
    Mockito.verify(productRepository).saveAndFlush(product);
    Mockito.verify(solrVendorCollectionService).updateImageQcResponseToSolr(product, productReviewer);
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(Mockito.any(), Mockito.anyBoolean());
  }

  @Test
   void updateStatesForAutoApproveCapableProductsTestContent() throws Exception {
    String notes = new String();
    product.setReviewType(ReviewType.CONTENT);
    product.setEdited(true);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    instance.updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void updateStatesForAutoApproveCapableProductsTestImage() throws Exception {
    String notes = new String();
    product.setReviewType(ReviewType.IMAGE);
    product.setEdited(true);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    instance.updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void updateStatesForAutoApproveCapableProductsTestContentAndImage() throws Exception {
    String notes = new String();
    product.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    product.setEdited(true);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    instance.updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void updateStatesForAutoApproveCapableProductsTestContentRevised() throws Exception {
    String notes = new String();
    product.setReviewType(ReviewType.CONTENT);
    product.setEdited(true);
    product.setRevised(true);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    instance.updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void updateStatesForAutoApproveCapableProductsTestReviewTypeNull() throws Exception {
    String notes = new String();
    product.setEdited(true);
    product.setRevised(true);
    product.setReviewType(null);
    ProductReviewer productReviewer = new ProductReviewer();
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    instance.updateStatesForAutoApproveCapableProducts(product, notes, productReviewer);
    Mockito.verify(this.productDistributionTaskService).findByProductId(Mockito.anyString());
  }

  @Test
   void publishAutoApprovalEventsTest() {
    instance.publishAutoApprovalEvents(false, product);
  }

  @Test
   void publishAutoApprovalEventsTrueTest() {
    product.setEdited(true);
    instance.publishAutoApprovalEvents(true, product);
    Mockito.verify(approvedProductPublisherService).publishEditedVendorApprovedEvent(product);
  }

  @Test
   void publishAutoApprovalEventsRevisedTest() {
    product.setRevised(true);
    instance.publishAutoApprovalEvents(true, product);
    Mockito.verify(approvedProductPublisherService).publishRevisedVendorApprovedEvent(product, false);
  }

  @Test
   void publishAutoApprovalEvedest() {
    product.setRevised(false);
    product.setEdited(false);
    instance.publishAutoApprovalEvents(true, product);
    Mockito.verify(approvedProductPublisherService).publishAutoApprovalEvent(Mockito.any());
  }

  @Test
    void quickApproveProductBrandInReviewRedistrictedKeyWordBlurImageTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setBrandApprovalStatus("IN_REVIEW");
    product.setRestrictedKeywordsPresent(true);
    product.setImageViolations("Blur");
    product.setTextViolations(Constants.PENDING);
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
  }

  @Test
    void quickApproveProductBrandCodeNullTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(null);
    product.setProductCode(PRODUCT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setBrandApprovalStatus("IN_REVIEW");
    product.setRestrictedKeywordsPresent(true);
    product.setImageViolations("Blur");
    product.setTextViolations("Blur");
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productUtils).getBrandCodeByBrandName(BRAND_NAME);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
  }

  @Test
    void quickApproveProductMandatoryAttributesNotFilledAndFamilyColourNotSelectedTest() throws Exception {
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    categoryDetailResponse.getCategoryAttributes().get(2).getAttribute().setMandatory(true);
    ProductItem productItem = new ProductItem();
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setName(Constants.FAMILY_COLOUR);
    productItem.setProductItemAttributes(List.of(productItemAttribute));
    product.setProductItems(List.of(productItem));
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    QuickApprovalResponse quickApprovalResponse =
        this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES, productReviewer,
          false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Assertions.assertEquals(5,
        quickApprovalResponse.getVendorQuickApprovalResponse().getErrorCodes().size());
  }

  @Test
    void quickApproveProductMandatoryAttributesNotFilledAndFamilyColourNotSelectedPassedVilationTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    productSystemParameterResponse.setValue("false");
    categoryDetailResponse.getCategoryAttributes().get(2).getAttribute().setMandatory(true);
    ProductItem productItem = new ProductItem();
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setName(Constants.FAMILY_COLOUR);
    productItem.setProductItemAttributes(List.of(productItemAttribute));
    product.setProductItems(List.of(productItem));
    product.setImageViolations("Pending");
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    QuickApprovalResponse quickApprovalResponse =
        this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES, productReviewer,
          false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Assertions.assertEquals(6,
        quickApprovalResponse.getVendorQuickApprovalResponse().getErrorCodes().size());
  }

  @Test
   void quickApproveProductTest() throws Exception {
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    Vendor vendor =
      new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    product.setVendorId(VENDOR_ID);
    product.setCurrentVendor(vendor);
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productImageRepository).findByProductId(Mockito.anyString());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.anyString());
  }

  @Test
   void quickApproveEditedProductTest() throws Exception {
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Vendor vendor =
      new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    product.setVendorId(VENDOR_ID);
    product.setCurrentVendor(vendor);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
        .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
  }

  @Test
   void quickApproveEditedProductAlreadyApprovedTest() throws Exception {
    Product product = getProduct();
    product.setMarkForDelete(true);
    product.setState(WorkflowState.PASSED);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Vendor vendor =
      new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    product.setVendorId(VENDOR_ID);
    product.setCurrentVendor(vendor);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
      .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
      .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
      product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    QuickApprovalResponse quickApprovalResponse =
      this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Assertions.assertEquals(
        Collections.singletonList(ApiErrorCode.PRODUCT_IS_ALREADY_APPROVED.getCode()),
        quickApprovalResponse.getVendorQuickApprovalResponse().getErrorCodes());
  }

  @Test
   void quickApproveEditedProductTestForEmptyProductAttributes() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setMarkForDelete(false);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setEdited(true);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    attributeValueResponse.setBrandApprovalStatus(BRAND_NOT_APPROVED);
    product.setProductAttributes(Collections.emptyList());
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
      .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
      .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
      product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    QuickApprovalResponse quickApprovalResponse =
      this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Assertions.assertEquals(ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode(),
        quickApprovalResponse.getVendorQuickApprovalResponse().getErrorCodes().get(0));
  }

  @Test
   void quickApproveEditedProductTestForEmptyProductImages() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    Product product = getProduct();
    product.setMarkForDelete(false);
    product.setId(PRODUCT_ID);
    product.setCurrentVendor(new Vendor());
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setProductAttributes(Collections.singletonList(new ProductAttribute()));
    product.setEdited(true);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    attributeValueResponse.setBrandApprovalStatus(BRAND_NOT_APPROVED);
    product.setProductAttributes(Collections.emptyList());
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
      .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
      .thenReturn(productDistributionTask);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
      product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    QuickApprovalResponse quickApprovalResponse =
      this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }


  @Test
   void quickApproveEditedProductProtectedBrandTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
        .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(productUtils.validateProtectedBrand(Mockito.anyString(), Mockito.any())).thenReturn(false);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
  }

  @Test
   void quickApproveEditedProductEmptyAttributes() throws Exception {
    Product product = createProductList().get(0);
    product.getProductAttributes().forEach(productAttribute -> productAttribute.setMarkForDelete(true));
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product.setState(WorkflowState.IN_REVIEW);
    Mockito.when(pbpFeign.updateReviewType(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode(), ReviewType.CONTENT.name()))
      .thenReturn(new GdnBaseRestResponse(true));
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
      .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
      product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    Mockito.when(productUtils.validateProtectedBrand(Mockito.anyString(), Mockito.any())).thenReturn(false);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
      productReviewer, false, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
  }

  @Test
   void quickApproveProductRemoveOriginalImageTest() throws Exception {
    Product product = createProductList().get(0);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    productSystemParameterResponse.setValue("false");
    product.getProductImages().add(new ProductImage(product, "location", 1, false));
    product.getProductImages().add(new ProductImage(product, "location", 0, true));
    product.getProductImages().get(0).setOriginalImage(true);
    product.getProductItems().add(new ProductItem());
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    product.setState(WorkflowState.IN_REVIEW);
    ProductItemImage image = new ProductItemImage();
    ProductItemImage image1 = new ProductItemImage();
    image.setSequence(1);
    image.setEdited(true);
    image.setOriginalImage(true);
    image1.setSequence(0);
    product.getProductImages().get(0).setEdited(false);
    product.getProductItems().get(0).getProductItemImages().add(image);
    product.getProductItems().get(0).getProductItemImages().add(image1);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
        product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
            pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
        productReviewer, false, product);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productImageRepository).findByProductId(Mockito.anyString());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.anyString());
  }

  @Test
   void getProductsBySellerCodeAndCategoryCodesTest() {
    Pageable pageable = PageRequest.of(0, 1);
    Mockito.when(
            productRepository.findByStoreIdAndBusinessPartnerCodeAndStateAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
                BUSINESS_PARTNER_CODE, WorkflowState.IN_REVIEW, ImmutableSet.of(CATEGORY_CODE), pageable))
        .thenReturn(productPage);
    instance.getProductsBySellerCodeAndCategoryCodes(STORE_ID, BUSINESS_PARTNER_CODE, WorkflowState.IN_REVIEW,
        ImmutableSet.of(CATEGORY_CODE), pageable);
    Mockito.verify(productRepository)
        .findByStoreIdAndBusinessPartnerCodeAndStateAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
            BUSINESS_PARTNER_CODE, WorkflowState.IN_REVIEW, ImmutableSet.of(CATEGORY_CODE), pageable);
  }

  @Test
   void updateProductRetryStatus_emptyStoreIdTest() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> instance.updateProductRetryStatus(StringUtils.EMPTY, PRODUCT_CODE,
        productRetryStatusUpdate));
  }

  @Test
   void updateProductRetryStatus_emptyProductCodeTest() throws Exception {
    Assertions.assertThrows(Exception.class,
      () -> instance.updateProductRetryStatus(STORE_ID, StringUtils.EMPTY,
        productRetryStatusUpdate));
  }

  @Test
   void updateProductRetryStatus_nullProductTest() throws Exception {
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate));
    } finally {
      Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    }
  }

  @Test
   void updateProductRetryStatusTest() throws Exception {
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    instance.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    // checkMs-1
    Mockito.verify(this.solrVendorCollectionService)
      .updateProductOnRetryStatusUpdate(Mockito.any());
    Assertions.assertTrue(productCaptor.getValue().isEdited());
    Assertions.assertTrue(productCaptor.getValue().isRevised());
  }

  @Test
   void updateProductRetryStatus_stateUpdateTest() throws Exception {
    productRetryStatusUpdate.setState(WorkflowState.NEED_CORRECTION.name());
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      ID)).thenReturn(Collections.singletonList(productDistributionTask));
    instance.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productDistributionTaskService).findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      product.getId());
    Mockito.verify(this.productDistributionTaskService)
      .saveProductDistributionTaskList(Mockito.anyList());
    Mockito.verify(this.solrVendorCollectionService)
      .updateProductOnRetryStatusUpdate(Mockito.any());
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Assertions.assertTrue(productCaptor.getValue().isEdited());
    Assertions.assertTrue(productCaptor.getValue().isRevised());
    Assertions.assertEquals(WorkflowState.NEED_CORRECTION, productCaptor.getValue().getState());
  }

  @Test
   void updateProductRetryStatus_stateUpdate_emptyTaskTest() throws Exception {
    productRetryStatusUpdate.setState(WorkflowState.IN_REVIEW.name());
    Mockito.when(this.productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      ID)).thenReturn(Collections.emptyList());
    Mockito.when(this.distributionTaskService.generateDistributionTaskForProduct(STORE_ID,
        product.getCurrentVendor(), Collections.singletonList(product), WorkflowState.IN_REVIEW))
      .thenReturn(Collections.singletonList(productDistributionTask));
    instance.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productDistributionTaskService).findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      product.getId());
    Mockito.verify(this.productDistributionTaskService)
      .saveProductDistributionTaskList(Mockito.anyList());
    Mockito.verify(this.solrVendorCollectionService)
      .updateProductOnRetryStatusUpdate(Mockito.any());
    Mockito.verify(this.productRepository).save(productCaptor.capture());
    Mockito.verify(this.distributionTaskService).generateDistributionTaskForProduct(STORE_ID,
        product.getCurrentVendor(), Collections.singletonList(product), WorkflowState.IN_REVIEW);
    Assertions.assertTrue(productCaptor.getValue().isEdited());
    Assertions.assertTrue(productCaptor.getValue().isRevised());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, productCaptor.getValue().getState());
  }

  @Test
   void quickApproveProductWithALlViolationAndBulkTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setBrandApprovalStatus("IN_REVIEW");
    product.setRestrictedKeywordsPresent(true);
    product.setImageViolations("Blur");
    product.setTextViolations(Constants.PENDING);
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
      productReviewer, true, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
  }

  @Test
   void quickApproveBulkActionProductTest() throws Exception {
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productAttribute.setAttributeType(ACTION_TYPE);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Vendor vendor =
      new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).isAbleToReject(false).isQcRequired(false).build();
    product.setVendorId(VENDOR_ID);
    product.setCurrentVendor(vendor);
    product.setId(PRODUCT_ID);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setBrandApprovalStatus(Constants.BRAND_APPROVAL_STATUS);
    product.setRestrictedKeywordsPresent(false);
    product.setCategoryCode(CATEGORY_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
      .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class))).thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(product.getId(),
      product.getCurrentVendor().getId())).thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product)).thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true)).thenReturn(product);
    Mockito.when(
        pbpFeign.findSystemParameter(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          Constants.YOUTUBE_URL_VALIDATION_SWITCH))
      .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
      productReviewer, true, product);
    Mockito.verify(this.productRepository).save(product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(productUtils).validateProtectedBrand(Mockito.anyString(), Mockito.any());
    Mockito.verify(productImageRepository).findByProductId(Mockito.anyString());
    Mockito.verify(productItemImageRepository).findByProductId(Mockito.anyString());
  }

  @Test
   void rejectProductByVendorBulkActionTest() throws Exception {
    RejectReasonDto rejectReasonDto =
            RejectReasonDto.builder().product(Collections.singletonList("Brand Invalue")).build();
    vendor.setAbleToReject(true);
    rejectProductDTO.setBulkAction(true);
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);
    this.newProduct.setCurrentVendor(vendor);
    productDistributionTask.setVendor(vendor);
    Mockito.when(mapper.writeValueAsString(any(NotesAndRejectReason.class))).thenReturn(NOTES);
    Mockito.when(this.productRepository.findById(Mockito.anyString())).thenReturn(Optional.of(this.newProduct));
    Mockito.when(this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
            .thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString())).thenReturn(productWorkflowStatusResponse);
    this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE);
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(newProduct.getProductCode());
    Mockito.verify(productBusinessPartnerService)
            .deleteProductCollection(GdnMandatoryRequestParameterUtil.getRequestId(),
                    GdnMandatoryRequestParameterUtil.getUsername(), rejectProductDTO);
    Mockito.verify(this.productDistributionTaskService).findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Mockito.verify(this.productRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).saveAndFlush(Mockito.any(Product.class));
    Mockito.verify(this.productRepository).save(Mockito.any(Product.class));
    Mockito.verify(this.productAttributeRespository)
            .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productImageRepository)
            .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemImageRepository)
            .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(productItemRepository)
            .deleteByProductId(newProduct.getId(), GdnMandatoryRequestParameterUtil.getUsername());
    Mockito.verify(this.productReviewerService).markForDeleteByProductCode(PRODUCT_CODE, DEFAULT_USERNAME);
    Mockito.verify(this.mapper).writeValueAsString(Mockito.any());
  }

  @Test
   void rejectProductByVendorAlreadyApprovedProductTestException() throws Exception {
    ReflectionTestUtils.setField(instance, "validateApprovedProductRejection", true);
    Vendor vendor =
      new Vendor.Builder().vendorCode(VENDOR_CODE).isAbleToReject(true).isQcRequired(false).build();
    productDistributionTask.setVendor(vendor);
    this.newProduct.setCurrentVendor(vendor);
    this.newProduct.setMarkForDelete(true);
    Mockito.when(this.productRepository.findById(Mockito.anyString()))
      .thenReturn(Optional.of(this.newProduct));
    Mockito.when(
      this.productDistributionTaskService.findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
        Mockito.anyString())).thenReturn(productDistributionTask);
    Mockito.when(this.productBusinessPartnerService.getWorkflowStatus(Mockito.anyString()))
      .thenReturn(productWorkflowStatusResponse);
    Assertions.assertThrows(Exception.class,
        () -> this.instance.rejectProductByVendor(rejectProductDTO, VENDOR_CODE));
    Mockito.verify(this.productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
      Mockito.verify(this.productDistributionTaskService)
          .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
  }

  @Test
   void quickApproveProductWithUnAssignedTest() throws Exception {
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setCurrentVendor(null);
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
      productReviewer, true, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
  }

  @Test
   void quickApproveProductWithAllItems_MFDTrue_Test() throws Exception {
    Product product = getProduct();
    product.getProductItems().forEach(productItem -> productItem.setMarkForDelete(true));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setCurrentVendor(new Vendor());
    productSystemParameterResponse.setValue("false");
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString())).thenReturn(product);
    Mockito.when(productServiceRepository.getCategoryDetailByCategoryCode(CATEGORY_CODE))
      .thenReturn(categoryDetailResponse);
    this.instance.quickApproveProduct(VENDOR_CODE, PRODUCT_CODE, ADDITIONAL_NOTES,
      productReviewer, true, product);
    Mockito.verify(this.productServiceRepository).getCategoryDetailByCategoryCode(CATEGORY_CODE);
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 0);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
      PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleTestWithSwitchOff() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", false);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 90);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(true);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
      PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyImagesTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 0);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "orphanedImageQuickApprovalAutoHealFlag", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());

  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImagesTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", false);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 1);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.setPostLive(true);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setProductAttributes(new ArrayList<>());
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    product.getProductItems().get(0).setProductItemImages(null);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImagesSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 1);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.setPostLive(true);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setProductAttributes(new ArrayList<>());
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    product.getProductItems().get(0).setProductItemImages(null);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImagesTest2() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 0);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    Product product = getProduct();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setProductAttributes(new ArrayList<>());
    ProductItem productItem = new ProductItem();
    productItem.setProduct(product);
    product.setProductItems(Collections.singletonList(productItem));
    product.getProductItems().get(0).setProductItemImages(null);
    product.setPostLive(true);
    product.setEdited(false);
    product.setRevised(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void testHealProductItemImages2() {
    Product product = getProduct();
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    ProductItem productItem2 = new ProductItem();
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productItems.add(productItem2);
    product.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void testHealProductItemImages() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE); // SKU code doesn't match
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    productItem1.setMarkForDelete(true);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void testHealProductItemImagesNonMAtch() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE.concat("1"));
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    productItem1.setMarkForDelete(true);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void testHealProductItemImagesNonMAtchFalse() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE.concat("1"));
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    productItem1.setMarkForDelete(false);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", false);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void testHealProductItemImagesSwitchOff() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", false);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }


  @Test
   void testHealProductItemImagesSwitchOnSameCode() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void testHealProductItemImagesSwitchOnSameCodeSwitchOff() {
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(SKU_CODE);
    Product productFromPcb = new Product();
    ProductItem productItem1 = new ProductItem();
    productItem1.setSkuCode(SKU_CODE);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem1);
    productFromPcb.setProductItems(productItems);
    boolean isItemDeleted = false;
    boolean activatedBefore = true;
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", false);
    instance.healProductItemImages(product, productFromPcb, isItemDeleted, activatedBefore);
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImagesTest_autoHeal() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withImageExcTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(
          Collections.singletonList(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode())).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productRepository.saveAndFlush(any())).thenReturn(product);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withNoErrorCodeTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "relaxEqualityCheckForMFDTrueItemAutoHeal", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withBrandInReviewTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.REJECTED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withBrandMarkForDeleteTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    attributeValueResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withPostLiveEdited() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setPostLive(true);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setOriginalImage(false);
    image.setActive(false);
    image.setMainImages(true);
    product.setPostLive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setImages(Collections.singletonList(image));
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.REJECTED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }

  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_emptyItemAttributeTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    Product product = getProduct();
    product.setPostLive(true);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH);
    image.setOriginalImage(false);
    image.setActive(false);
    image.setMainImages(true);
    product.setPostLive(true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.setEdited(false);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setImages(Collections.singletonList(image));
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
        VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.REJECTED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
        productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
            anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
        quickApprovalResponse);
    Mockito.verify(productServiceRepository)
        .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }


  @Test
   void autoHealProductDataForApprovalIfEligible_autoHealForEmptyItemImages_withNoBrandTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 30);
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    Product product = getProduct();
    product.setPostLive(true);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(null);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.REJECTED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(Mockito.anyString()))
      .thenReturn(product);
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(fileStorageService).isSourceImageFileExist(any());
  }

  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImagesEmpty_forQuickApproval() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance,"validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.getProductItems().get(0).setProductItemImages(null);

    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setPostLive(true);
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Product responseProduct = getProduct();
    product.setPostLive(true);
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);

    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }


  @Test
   void testGetAllProductDetailsByCodeAutoHealOnProductImagesEmpty_forValidImagesInGcs() throws Exception {
    ReflectionTestUtils.setField(instance, "autoHealProductData", true);
    ReflectionTestUtils.setField(instance,"validateAndHealQuickApprovalEnabled",true);
    Product product = getProduct();
    product.getProductItems().get(0).setSkuCode(CODE);
    product.setProductCode(PRODUCT_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setName(NOTES);
    product.setProductAttributes(Collections.singletonList(productAttribute));
    product.getProductItems().get(0).setProductItemImages(null);

    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setPostLive(true);
    productDetailResponse.setEdited(true);
    productDetailResponse.setRevised(true);
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    Mockito.when(productBusinessPartnerService.getWorkflowStatus(product.getProductCode()))
      .thenReturn(new ProductWorkflowStatusResponse(product.getProductCode(),
        Collections.singletonList("ACTIVE"), Collections.emptyMap()));
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isSourceImageFileExist(Mockito.anyString())).thenReturn(true);
    Product responseProduct = getProduct();
    product.setPostLive(true);
    responseProduct.setProductCode(PRODUCT_CODE);
    this.instance.autoHealProductData(product, Constants.AUTOHEAL);

    Mockito.verify(productServiceRepository).getProductDetailByProductCode(PRODUCT_CODE, true ,
      true);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productRepository).saveAndFlush(productArgumentCaptor.capture());
    Mockito.verify(productBusinessPartnerService).getWorkflowStatus(product.getProductCode());
    Assertions.assertTrue(
        CollectionUtils.isNotEmpty(productArgumentCaptor.getValue().getProductItems()));
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setOriginalImage(null);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    productImage.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.setProductImages(List.of(productImage));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_originalFalse() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setOriginalImage(false);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setProduct(product);
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.setProductImages(Arrays.asList(productImage, productImage2));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_InActiveoriginalFalse() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    attributeValueResponse.setMarkForDelete(true);
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setOriginalImage(false);
    productImage.setMarkForDelete(false);
    productImage.setActive(false);
    ProductImage productImage2 = new ProductImage();
    productImage2.setProduct(product);
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(false);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.setProductImages(Arrays.asList(productImage, productImage2));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_ItemOriginalFalse() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ReflectionTestUtils.setField(instance, "considerActivatedBeforeForAutoHeal", false);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setProductItems(Collections.singletonList(new ProductItem()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setOriginalImage(false);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setProductItem(product.getProductItems().get(0));
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productImage,
      productImage2));

    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_ItemOriginalFalseForInvalidBrand() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    doThrow(ApplicationRuntimeException.class).when(productServiceRepository).getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
      anyString(), anyBoolean());
    Product product = createProductList().get(0);
    product.setProductItems(Collections.singletonList(new ProductItem()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setOriginalImage(false);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setProductItem(product.getProductItems().get(0));
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productImage,
      productImage2));

    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_InActiveItemOriginalFalse() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setProductItems(Collections.singletonList(new ProductItem()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setOriginalImage(false);
    productImage.setMarkForDelete(false);
    productImage.setActive(false);
    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setProductItem(product.getProductItems().get(0));
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(false);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productImage,
      productImage2));

    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }
  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_ItemOriginalNull() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setProductItems(Collections.singletonList(new ProductItem()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setOriginalImage(null);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setProductItem(product.getProductItems().get(0));
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productImage,
      productImage2));

    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcsTest_InActiveItemOriginalNull() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setProductItems(Collections.singletonList(new ProductItem()));
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setOriginalImage(null);
    productImage.setMarkForDelete(false);
    productImage.setActive(false);
    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setProductItem(product.getProductItems().get(0));
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(false);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().get(0).setProductItemImages(Arrays.asList(productImage,
      productImage2));

    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForItemImagesGcsTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(productServiceRepository.getProductDetailByProductCode(PRODUCT_CODE, true, true)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isSourceImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    productImage.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    productImage.setOriginalImage(null);
    ProductImage productImage2 = new ProductImage();
    productImage2.setProduct(product);
    productImage2.setOriginalImage(true);
    productImage2.setMarkForDelete(false);
    productImage2.setActive(true);
    productImage2.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.setProductImages(Arrays.asList(productImage, productImage2));
    Product data = this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcs_mfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ReflectionTestUtils.setField(instance, "orphanedImageQuickApprovalAutoHealFlag", true);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
      PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setOriginalImage(true);
    productImage.setMarkForDelete(false);
    productImage.setActive(true);
    productImage.setLocationPath("/"+LOCATION_PATH+".jpeg");
    productImage.setEdited(true);
    product.setProductImages(List.of(productImage));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
    Mockito.verify(productRepository).save(Mockito.any(Product.class));
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForGcs_ItemImagesMfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
      PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isFinalImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductItemImage productImage = new ProductItemImage();
    productImage.setProductItem(product.getProductItems().get(0));
    productImage.setMarkForDelete(true);
    productImage.setActive(true);
    product.setProductAttributes(null);
    productImage.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.getProductItems().forEach(productItem -> productItem.getProductItemImages().forEach(i -> i.setMarkForDelete(true)));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void autoHealProductDataForApprovalIfEligibleForItemImagesGcs_mfdTrueTest() throws Exception {
    ReflectionTestUtils.setField(instance, "validateAndHealQuickApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "autoHealAutoApprovalProductData", true);
    ReflectionTestUtils.setField(instance, "maxImageCountForGcsFileCheck", 20);
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductItemResponses().iterator().next().setSkuCode(CODE);
    productDetailResponse.getProductItemResponses().iterator().next().setActivated(false);
    PredefinedAllowedAttributeValueResponse attributeValueResponse =
      new PredefinedAllowedAttributeValueResponse();
    VendorQuickApprovalResponse quickApprovalResponse =
      VendorQuickApprovalResponse.builder().errorCodes(Collections.emptyList()).build();
    attributeValueResponse.setValue(BRAND_CODE);
    attributeValueResponse.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    Mockito.when(distributionTaskService.getProductDetailByProductCode(BeanConfiguration.USER_NAME,
      PRODUCT_CODE)).thenReturn(productDetailResponse);
    Mockito.when(fileStorageService.isSourceImageFileExist(IMAGE_LOCATION+LOCATION_PATH)).thenReturn(true);
    Mockito.when(
      productServiceRepository.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(),
        anyString(), anyBoolean())).thenReturn(attributeValueResponse);
    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);
    product.setProductCode(PRODUCT_CODE);
    product.setBrand(BRAND_NAME);
    product.setBrandCode(BRAND_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    ProductImage productImage = new ProductImage();
    productImage.setProduct(product);
    productImage.setMarkForDelete(true);
    productImage.setActive(false);
    productImage.setLocationPath(IMAGE_LOCATION+LOCATION_PATH);
    product.setProductImages(List.of(productImage));
    this.instance.autoHealProductDataForApprovalIfEligible(PRODUCT_CODE, product,
      quickApprovalResponse);
    Mockito.verify(productServiceRepository)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(any(), anyString(), anyBoolean());
  }

  @Test
   void deleteOriginalImagesForProductAndItemsSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(instance, "deleteOriginalImages", false);
    instance.deleteOriginalImagesForProductAndItems(product);
    Assertions.assertNotNull(product.getProductImages());
  }

  @Test
   void pdtHistoryUpdateThroughEventsTest() throws Exception {
    ReflectionTestUtils.setField(instance,"pdtHistoryUpdateThroughEvent",true);
    productReviewers.get(0).setApproverAssignee(StringUtils.EMPTY);
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES))
        .thenReturn(productReviewers);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product1);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(eq(ID1))).thenReturn(TASK_CODE);
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, ASSIGN, ASSIGNED_TO, new Date());
    Mockito.verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productReviewerService).save(productReviewer);
    Mockito.verify(productDistributionTaskRepository).getTaskCodeForProduct(eq(ID1));
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PDT_PRODUCT_HISTORY_EVENT),
        Mockito.any());
  }

  @Test
   void pdtHistoryUpdateThroughEventsProductNullTest() throws Exception {
    ReflectionTestUtils.setField(instance,"pdtHistoryUpdateThroughEvent",true);
    productReviewers.get(0).setApproverAssignee(StringUtils.EMPTY);
    Mockito.when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES))
        .thenReturn(productReviewers);
    Mockito.when(productRepository.findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productDistributionTaskRepository.getTaskCodeForProduct(eq(ID1))).thenReturn(TASK_CODE);
    this.instance.doVendorProductAction(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, ASSIGN, ASSIGNED_TO, new Date());
    Mockito.verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODES);
    Mockito.verify(productReviewerService).save(productReviewer);
    Mockito.verify(productRepository).findByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
  }

  @Test
   void testInit() {
    instance.init();
  }

  @Test
   void checkIfVendorAutoHealKeyExistsTest() {
    instance.checkIfVendorAutoHealKeyExists(PRODUCT);
  }

  @Test
   void cacheVendorAutoHealKeyTest() {
    instance.cacheVendorAutoHealKey(PRODUCT);
  }

  @Test
   void updateBrandOfProductTest() throws Exception {
    ProductItem productItem = new ProductItem();
    ProductAttribute productAttribute =
        new ProductAttribute(product2, ATTRIBUTE_CODE1, "NAME", "VALUE", DEFINING_ATTRIBUTE);
    productItem.setProductItemAttributes(Collections
        .singletonList(new ProductItemAttribute(STORE_ID, ATTRIBUTE_CODE1, "NAME", "VALUE", PREDEFINED_ATTRIBUTE)));
    product.setProductItems(Collections.singletonList(productItem));
    product.setProductAttributes(Collections.singletonList(productAttribute));
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest(PRODUCT_CODE, BRAND_CODE, BRAND_NAME);
    instance.updateBrandOfProduct(changeBrandRequest);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepository).save(any(Product.class));
  }

  @Test
   void updateBrandOfProductWithProductAsNullTest() throws Exception {
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(null);
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest(PRODUCT_CODE, BRAND_CODE, BRAND_NAME);
    instance.updateBrandOfProduct(changeBrandRequest);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
  }

  @Test
   void updateBrandOfProductWithEmptyProductAttributesTest() throws Exception {
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributes(Collections
        .singletonList(new ProductItemAttribute(STORE_ID, ATTRIBUTE_CODE1, "NAME", "VALUE", PREDEFINED_ATTRIBUTE)));
    product.setProductItems(Collections.singletonList(productItem));
    Mockito.when(productRepository.findByProductCode(PRODUCT_CODE)).thenReturn(product);
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest(PRODUCT_CODE, BRAND_CODE, BRAND_NAME);
    instance.updateBrandOfProduct(changeBrandRequest);
    Mockito.verify(productRepository).findByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepository).save(any(Product.class));
  }

  @Test
   void fetchNeedCorrectionProductsTest() {
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(productRepository.findByStoreIdAndStateAndUpdatedDateBetweenAndMarkForDeleteTrue(Mockito.anyString(),
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(productPage);
    instance.fetchNeedCorrectionProducts(STORE_ID, new Date(), new Date(), pageable);
    Mockito.verify(productRepository)
        .findByStoreIdAndStateAndUpdatedDateBetweenAndMarkForDeleteTrue(Mockito.anyString(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
   void publishInternalHistoryEventTest() {
    InternalHistoryEventModel internalHistoryEventModel = new InternalHistoryEventModel();
    internalHistoryEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(PRODUCT);
    instance.publishInternalHistoryEventForProduct(internalHistoryEventModel);
    Mockito.verify(kafkaProducer).send(PRODUCT, PRODUCT_CODE, internalHistoryEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getInternalHistoryEventName();
  }

  @Test
   void testUpdateAppealProduct() throws Exception{
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    appealProductRequest.setProductCode(PRODUCT_CODE);
    appealProductRequest.setNotes(FINAL_NOTES);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    product.setReviewType(ReviewType.CONTENT);
    Mockito.when(productRepository.findByProductCode(appealProductRequest.getProductCode()))
        .thenReturn(product);
    Mockito.when(appealProductService.findAppealProductByProductCode(anyString())).thenReturn(null);
    instance.updateAppealProduct(appealProductRequest, STORE_ID);
    verify(productRepository).save(product);
    verify(productRepository).findByProductCode(product.getProductCode());
    verify(appealProductService).upsertAppealProduct(any(AppealedProduct.class));
    verify(solrReindexPublisherService).publishPDTProductApprovalToSolr(any(
        PDTProductUpdateProductToSolrEventModel.class));
    verify(appealProductService).findAppealProductByProductCode(PRODUCT_CODE);
  }

  @Test
   void testUpdateAppealNullProduct() throws Exception {
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    appealProductRequest.setProductCode(PRODUCT_CODE);
    appealProductRequest.setNotes(FINAL_NOTES);
    Mockito.when(productRepository.findByProductCode(appealProductRequest.getProductCode()))
      .thenReturn(null);
    Mockito.when(appealProductService.findAppealProductByProductCode(anyString())).thenReturn(null);
    AppealProductResponse appealProductResponse =
      instance.updateAppealProduct(appealProductRequest, STORE_ID);
    verify(productRepository).findByProductCode(product.getProductCode());
    Assertions.assertEquals(appealProductResponse.getErrorCode(),
        ApiErrorCode.PRODUCT_NOT_FOUND.getCode());
  }

  @Test
   void testUpdateAppealInvalidStateProduct() throws Exception {
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    appealProductRequest.setProductCode(PRODUCT_CODE);
    appealProductRequest.setNotes(FINAL_NOTES);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productRepository.findByProductCode(appealProductRequest.getProductCode()))
      .thenReturn(product);
    Mockito.when(appealProductService.findAppealProductByProductCode(anyString())).thenReturn(null);
    AppealProductResponse appealProductResponse =
      instance.updateAppealProduct(appealProductRequest, STORE_ID);
    verify(productRepository).findByProductCode(product.getProductCode());
    Assertions.assertEquals(appealProductResponse.getErrorCode(),
        ApiErrorCode.PRODUCT_IS_IN_INVALID_STATE.getCode());
  }

  @Test
  void updateAndApproveProductTest_BrandEmptyValidationTest() throws Exception {
    ReflectionTestUtils.setField(instance, "brandExistCheckEnabled", true);

    ProductDistributionTask productDistributionTask = new ProductDistributionTask(
        TASK_CODE,
        new Vendor(),
        this.product,
        WorkflowState.IN_REVIEW,
        new Date()
    );

    Product product = createProductList().get(0);
    product.setId(PRODUCT_ID);

    Mockito.when(this.productDistributionTaskService.findByProductId(Mockito.anyString()))
        .thenReturn(productDistributionTask);
    Mockito.when(this.productRepository.findByProductCodeAndMarkForDeleteFalse(anyString()))
        .thenReturn(product);
    Mockito.when(this.productRepository.save(any(Product.class)))
        .thenReturn(product);
    Mockito.when(this.productDistributionTaskService.getWorkflowStatePostApproval(
            product.getId(), product.getCurrentVendor().getId()))
        .thenReturn(WorkflowState.PASSED);
    Mockito.when(this.productUtils.replaceProductImageDetails(product, product))
        .thenReturn(product);
    Mockito.when(this.productUtils.replaceProductDetails(product, product, true))
        .thenReturn(product);
    Mockito.when(this.pbpFeign.findSystemParameter(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(),
            Constants.YOUTUBE_URL_VALIDATION_SWITCH))
        .thenReturn(new GdnRestSingleResponse<>(productSystemParameterResponse, REQUEST_ID));

    // Simulate brand not found (brandCode is null)
    Mockito.when(this.productUtils.getBrandCodeByBrandName(Mockito.anyString()))
        .thenReturn(null);

    // Act & Assert
    ApplicationRuntimeException exception = Assertions.assertThrows(
        ApplicationRuntimeException.class,
        () -> instance.updateAndApproveProduct(VENDOR_CODE, product, ADDITIONAL_NOTES, false, productReviewer)
    );

    Assertions.assertEquals("Can not process invalid input data :Brand does not exist, provide a valid brand name and try again.", exception.getErrorMessage());
    Mockito.verify(this.productUtils).getBrandCodeByBrandName(Mockito.anyString());
    Mockito.verify(this.productRepository).findByProductCodeAndMarkForDeleteFalse(Mockito.any());
  }

  @Test
  public void findProductByProductCodeTest() {
    instance.findProductByProductCode(PRODUCT_CODE2);
    Mockito.verify(this.productRepository).findByProductCode(PRODUCT_CODE2);
  }
}
