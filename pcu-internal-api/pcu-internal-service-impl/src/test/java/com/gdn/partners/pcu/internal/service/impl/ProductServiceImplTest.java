package com.gdn.partners.pcu.internal.service.impl;

import com.gda.mta.product.dto.ItemNeedRevisionNotes;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.feign.BPJPHFeign;
import com.gdn.partners.pcu.internal.client.feign.MarginFeign;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.client.model.request.Margin;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHData;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCeritficationDetails;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductDetailCompleteResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ExtCatalogProperties;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.ProductWorkflowService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.ProductAttributeFeedbackEventModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterProductDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.SelectedMasterProductDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.enums.Source;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SelectedMasterProductDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeCheckResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductReviewerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductRevisionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

/**
 * Created by govind on 14/01/2019 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductServiceImplTest {

  private static final String MASTER_PRODUCT_ID = "master-product-id";
  private static final String DUPLICATE_PRODUCT_ID = "duplicate-product-id";
  private static final String PRODUCT_ID = "product-id";
  private static final String PRODUCT_SKU = "productSku";
  private static final String BARCODE = "barcode";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String HALAL_PRODUCT_PREFIX = "https://www.blibli.com/p/product-detail/ps--";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String DESC_ATTR_VALUE = "desc-attr-value";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String REQUEST_ID = "requestId";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String NOTES = "Diubah : [{field: 'Unique Selling Point', oldValue: usp, newValue: uspusp}]";
  private static final String NOTES_ASSERTION = "{field: 'Unique Selling Point', oldValue: 'usp', newValue: 'uspusp'}";
  private static final String BULK_ACTION = "bulkAction";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String REJECT_REASON = "rejectReason";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_TYPE = "3";
  private static final String UPC_CODE = "upcCode";
  private static final String CATEGORY = "handPhone";
  private static final String CATEGORY_ID = "categoryId";
  private static final String DEFAULT_BRAND_VALUE = "nike";
  private static final String BRAND = "brand";
  private static final String KEYWORD = "keyword";
  private static final String ID = "id";
  private static final String ID1 = "id1";
  private static final String ID2 = "id2";
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CERTIFICATE_NUMBER = "certificationNumber";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final Date CREATED_DATE = new Date();
  private static final Date UPDATED_DATE = new Date();
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String DEFAULT_STORE_ID = "12345";
  private static final String DEFAULT_CATEGORY_CODE = "CGC-001";
  private static final String DEFAULT_CATEGORY_ID = "CGI-001";

  private static final String B2C_RETAIL = "B2C_RETAIL";
  private static final String DEFAULT_CATEGORY_ID_2 = "CGI-002";
  private static final String DEFAULT_CREATED_BY = "Creator";
  private static final Integer DEFAULT_LOGISTIC_ADJUSTMENT = 1;
  private static final String DEFAULT_NAME = "Name";
  private static final Integer DEFAULT_SEQUENCE = 1;
  private static final Long DEFAULT_VERSION = 1L;
  private static final String DEFAULT_DESCRIPTION = "Description";
  private static final String DEFAULT_STATE = "State";
  private static final String DEFAULT_REASON = "reason";
  private static final String DEFAULT_UPDATED_BY = "Updator";
  private static final String DEFAULT_ATTRIBUTE_CODE = "ATC-001";
  private static final String DEFAULT_ATTRIBUTE_TYPE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String DEFAULT_ATTRIBUTE_ID = "AID-001";
  private static final String CATEGORY_CHANGE_NOTES = "Category changed from categoryCode to CGC-001";
  private static final String USER_NAME_2 = "userName 2";
  private static final String HISTORY_DESCRIPTION = "Diubah";
  private static final String ACTION = "SUSPEND";
  private static final String MERCHANT_CODE = "ABC-1000001";
  private static final String ITEM_NAME = "ITEM_NAME";
  private static final String ITEM_NAME_1 = "ITEM_NAME_1";
  private static final String ITEM_NAME_2 = "ITEM_NAME_2";
  private static final String SKU_CODE = "SKU_CODE";
  private static final String SKU_CODE_1 = "SKU_CODE_1";
  private static final String SKU_CODE_2 = "SKU_CODE_2";
  private static final String ITEM_ATTRIBUTE_VALUE = "Value A";
  private static final String ITEM_ATTRIBUTE_VALUE_1 = "Value B";
  private static final String WARNA_ATTRIBUTE_NAME = "warna";

  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String USERNAME = "username";
  private static final String TYPE = "SUSPEND";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String DEFAULT_SORT_TYPE = "desc";
  public static final String clientId = "clientId";
  private final String ACTIVE = "ACTIVE";
  private final String FINAL_QC = "FINAL_QC";
  private final String STATE = "1";
  private static final String WHOLESALE_CONFIG_TYPE = "CONFIG_TYPE";
  private static final int QUANTITY = 3;
  private static final double PERCENTAGE = 10.0;
  private static final int QUANTITY_2 = 4;
  private static final double PERCENTAGE_2 = 12.0;
  private static final String HALAL_API_KEY = "api-key";

  private static final String INTERNAL_BULK_UPLOAD = "internal";
  private static final String WHOLESALE_CONFIGURATIONS_MISMATCH_MESSAGE =
      "Wholesale rules config fall in different tiers";
  private static final String DEFINING_ATTRIBUTE_MISMATCH_MESSAGE = "Defining attributes mismatch";
  private static final String BUSINESS_PARTNER_CODE_1 = "BUSINESS_PARTNER_CODE_1";
  private static final String BUSINESS_PARTNER_NAME_1 = "BUSINESS_PARTNER_NAME_1";
  private static final String BUSINESS_PARTNER_CODE_2 = "BUSINESS_PARTNER_CODE_2";
  private static final String BUSINESS_PARTNER_NAME_2 = "BUSINESS_PARTNER_NAME_2";
  public static final String CM = "CM";
  public static final String CC = "CC";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");
  private static final List<String> VENDOR_ERROR_NOTES = Arrays.asList("correctionReason");
  private static final String CERTIFICATION_NUMBER = "certificationNumber";
  private static final String ACTIVITY = "activity";
  private static final String PREVIOUS_VALUE = "previousValue";
  private static final String CURRENT_VALUE = "currentValue";
  private static final String CURATION_STATUS = "curationStatus";
  private static final String DETAIL_PAGE_URL = "https://www.blibli.com/p/product-detail/pc--MTA-0000001";
  private static final String DS_EXTRACTED_ATTRIBUTE_VALUE1 =  "value1";
  private static final String DS_EXTRACTED_ATTRIBUTE_VALUE2 =  "value2";
  private static final String PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME =
      "productAttribtueExtractionsEventName";

  private byte[] fileContent;
  private MockMultipartFile multipartFile;

  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";
  private ReviewProductsFilterRequest reviewProductsFilterRequest;
  private SummaryFilterRequest summaryFilterRequest;
  private List<ReviewProductResponse> reviewProductResponses;
  private ProductDetailResponse productDetailResponse;
  private GdnRestSingleResponse<ProductDetailResponse> response;
  private GdnRestSingleResponse<ProductDetailCompleteResponse> productCompleteResponseGdnRestSingleResponse;
  private MasterAttributeResponse masterAttributeResponse;
  private GdnRestSingleResponse<MasterAttributeResponse> masterAttributeResponseGdnRestSingleResponse;
  private FilterCountResponse filterCountResponse;
  private ProductHistoryResponse productHistoryResponse;
  private ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private ProductSuggestionWebRequest productSuggestionWebRequest;
  private List<ProductSuggestionWebRequest> productSuggestionWebRequests;
  private Map<String, Set<String>> pristineSupportedCategoryMap;
  private Set<String> categorySet;
  private List<ProductCodeResponse> productCodeResponses;
  private ProductCodeResponse productCodeResponse;
  private ProductRequest productRequest;
  private HashMap<String, String> categoryFinalMap;
  private AttributeReqModel attributeReqModel;
  private CategoryDetailResponse categoryDetailResponse;
  private CategoryDetailResponse categoryDetailResponse1;
  private ProductSuspensionFilterRequest productSuspensionFilterRequest;
  private SummaryFilterRequest summaryFilterRequest1;
  private GdnRestListResponse<SuspensionProductResponse> gdnRestListResponse;
  private SuspensionProductResponse suspensionProductResponse;
  private ProductSuspensionHistoryResponse productSuspensionHistoryResponse;
  private BusinessPartnerCodesRequest businessPartnerCodesRequest;
  private GdnRestListResponse<MerchantNameResponse> merchantNameResponseGdnRestListResponse;
  private MerchantNameResponse merchantNameResponse;
  private ProductDetailResponse productDetailResponseGroupByWarna;
  private GdnRestSingleResponse<ProductDetailResponse> responseGroupByWarna;
  private ProductItemResponse productItemResponse = new ProductItemResponse();
  private ProductItemResponse productItemResponse1 = new ProductItemResponse();
  private ProductItemResponse productItemResponse2 = new ProductItemResponse();
  private Set<ProductItemResponse> productItemWebResponseSet = new HashSet<>();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse =
      new ProductItemAttributeValueResponse();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse1 =
      new ProductItemAttributeValueResponse();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse2 =
      new ProductItemAttributeValueResponse();
  private AttributeResponse attributeResponse = new AttributeResponse();
  private Set<ProductItemAttributeValueWebResponse> productItemAttributeValueWebResponseSet = new HashSet<>();
  private SummaryFilterWebRequest summaryFilterWebRequest;
  private ProductCollectionResponse productCollectionResponse;
  private HalalProductHistoryResponse halalProductHistoryResponse;
  private HalalProductHistoryWebResponse halalProductHistoryWebResponse;
  private GdnRestListResponse<ProductCollectionResponse> productCollectionResponseGdnRestListResponse;
  private CountWebRequest countWebRequest ;
  private SelectedMasterProductDownloadWebRequest selectedMasterProductDownloadWebRequest;
  private TaskHistoryResponse taskHistoryResponse;
  private WholesaleMappingResponse wholesaleMappingResponse = new WholesaleMappingResponse();
  private WholesaleMappingResponse wholesaleMappingResponse2 = new WholesaleMappingResponse();
  private WholesaleConfigResponse wholesaleConfigResponse = new WholesaleConfigResponse();
  private GdnRestSingleResponse<CategoryNamesResponse> categoryNamesResponseGdnRestSingleResponse;
  private GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse;
  private Map<String, ProfileResponse> profileResponseMap;
  private ProfileResponse profileResponse;
  private ProductDetailCompleteResponse productDetailCompleteResponse = new ProductDetailCompleteResponse();
  private MarginCategoryResponse marginCategoryResponse = new MarginCategoryResponse();
  private ProductAttributeRequest productAttributeRequestDSExtractedPredefined =
      new ProductAttributeRequest();
  private ProductAttributeValueRequest productAttributeValueRequestDSExtractedPredefined =
      new ProductAttributeValueRequest();
  private ProductAttributeRequest productAttributeRequestDSExtractedDescriptive =
      new ProductAttributeRequest();
  private ProductAttributeValueRequest productAttributeValueRequestDSExtractedDescriptive =
      new ProductAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequestDSExtractedDescriptive2 =
      new ProductAttributeValueRequest();
  private PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequestDSExtracted =
      new PredefinedAllowedAttributeValueRequest();
  private ProductAttributeResponse productAttributeResponseDSExtractedPredefined =
      new ProductAttributeResponse();
  private ProductAttributeValueResponse productAttributeValueResponseDSExtractedPredefined =
      new ProductAttributeValueResponse();
  private PredefinedAllowedAttributeValueResponse
      predefinedAllowedAttributeValueResponseDSExtracted =
      new PredefinedAllowedAttributeValueResponse();
  private ProductAttributeResponse productAttributeResponseDSExtractedDescriptive =
      new ProductAttributeResponse();
  private ProductAttributeValueResponse productAttributeValueResponseDSExtractedDescriptive =
      new ProductAttributeValueResponse();

  private ProductAttributeRequest productAttributeRequestDSExtractedPredefinedMultiValue =
      new ProductAttributeRequest();
  private PredefinedAllowedAttributeValueRequest
      predefinedAllowedAttributeValueRequestDSExtracted2 =
      new PredefinedAllowedAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequestDSExtractedPredefined2 =
      new ProductAttributeValueRequest();

  private ProductAttributeResponse productAttributeResponseDSExtractedPredefinedMultiValue =
      new ProductAttributeResponse();
  private ProductAttributeValueResponse productAttributeValueResponseDSExtractedPredefined2 =
      new ProductAttributeValueResponse();
  private PredefinedAllowedAttributeValueResponse
      predefinedAllowedAttributeValueResponseDSExtracted2 =
      new PredefinedAllowedAttributeValueResponse();

  private ProductAttributeRequest productAttributeRequestDSExtractedDescriptiveMultiValue =
      new ProductAttributeRequest();
  private ProductAttributeResponse productAttributeResponseDSExtractedDescriptiveMultiValue =
      new ProductAttributeResponse();
  private ProductAttributeValueResponse productAttributeValueResponseDSExtractedDescriptive2 =
      new ProductAttributeValueResponse();

  @Captor
  private ArgumentCaptor<List<AttributeReqModel>> attributeReqModels;

  @Captor
  private ArgumentCaptor<FilterMarginsByOrderItemsRequest> filterMarginsByOrderItemsRequestArgumentCaptor;

  @Mock
  private ProductWorkflowService productWorkflowService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PDTFeign pdtFeign;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private BPService bpService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @InjectMocks
  private ProductServiceImpl productService;

  @Mock
  private ProductSuggestionServiceImpl extCatalogService;

  @Mock
  private ExtCatalogProperties extCatalogProperties;

  @Mock
  private CategoryServiceImpl categoryService;

  @Mock
  private CacheProductServiceImpl cacheProductService;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private MarginFeign marginFeign;

  @Mock
  private BPJPHFeign bpjphFeign;

  @Captor
  private ArgumentCaptor<SummaryFilterRequest> summaryFilterRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductHistoryRequest> productHistoryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProductSuspensionRequest> bulkProductSuspensionRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<MasterDataBulkUpdateRequest> masterDataBulkUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<DistributionTaskMultipleFilterRequest> distributionTaskMultipleFilterRequestArgumentCaptor;

  private ProductReturnForCorrectionRequest productReturnForCorrectionRequest;

  @Captor
  private ArgumentCaptor<SelectedMasterProductDownloadRequest> selectedMasterProductDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<MasterProductDownloadRequest> masterProductDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryMultipleIdRequest> categoryMultipleIdRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<PDTNeedRevisionEventModel> pdtNeedRevisionEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<ScreeningProductBulkActionsRequest> screeningProductBulkActionsRequestArgumentCaptor;


  @BeforeEach
  public void init() {
    productReturnForCorrectionRequest = new ProductReturnForCorrectionRequest();
    productRequest = new ProductRequest();
    productRequest.setProductCode(PRODUCT_CODE);

    List<ProductAttributeRequest> productAttributes = new ArrayList<>();
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest.setSkuValue(false);
    List<AllowedAttributeValueRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueRequest);
    attributeRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueRequest);
    attributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setSequence(2);
    List<ProductAttributeValueRequest> productAttributeValues = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setDescriptiveAttributeValue(DESC_ATTR_VALUE);
    productAttributeValues.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValues);

    ProductAttributeRequest productPreDefinedAttributeRequest = new ProductAttributeRequest();
    AttributeRequest preDefinedAttributeRequest = new AttributeRequest();
    preDefinedAttributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    preDefinedAttributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    preDefinedAttributeRequest.setSkuValue(false);
    preDefinedAttributeRequest.setAllowedAttributeValues(allowedAttributeValues);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueRequest);
    preDefinedAttributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productPreDefinedAttributeRequest.setAttribute(preDefinedAttributeRequest);
    productPreDefinedAttributeRequest.setSequence(2);
    List<ProductAttributeValueRequest> productPredefinedAttributeValues = new ArrayList<>();
    ProductAttributeValueRequest productPreDefinedAttributeValueRequest = new ProductAttributeValueRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest1 =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    productPreDefinedAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest1);
    productPredefinedAttributeValues.add(productPreDefinedAttributeValueRequest);
    productPreDefinedAttributeRequest.setProductAttributeValues(productPredefinedAttributeValues);


    ProductAttributeRequest productDefinedAttributeRequest = new ProductAttributeRequest();
    AttributeRequest definedAttributeRequest = new AttributeRequest();
    definedAttributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    definedAttributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    definedAttributeRequest.setSkuValue(false);
    definedAttributeRequest.setAllowedAttributeValues(allowedAttributeValues);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueRequest);
    preDefinedAttributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productDefinedAttributeRequest.setAttribute(definedAttributeRequest);
    productDefinedAttributeRequest.setSequence(2);

    List<ProductAttributeValueRequest> productDefinedAttributeValues = new ArrayList<>();
    ProductAttributeValueRequest productDefinedAttributeValueRequest = new ProductAttributeValueRequest();
    AllowedAttributeValueRequest allowedAttributeValueRequest1 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest1.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    productDefinedAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest1);

    productDefinedAttributeValues.add(productDefinedAttributeValueRequest);
    productDefinedAttributeRequest.setProductAttributeValues(productDefinedAttributeValues);


    productAttributes.add(productAttributeRequest);
    productAttributes.add(productPreDefinedAttributeRequest);
    productAttributes.add(productDefinedAttributeRequest);
    productRequest.setProductAttributes(productAttributes);

    reviewProductsFilterRequest = new ReviewProductsFilterRequest();
    reviewProductsFilterRequest.setAssignedTo(ASSIGNED_TO);
    reviewProductsFilterRequest.setCategoryCode(CATEGORY_CODE);
    summaryFilterRequest = new SummaryFilterRequest();
    summaryFilterRequest.setAssignedTo(ASSIGNED_TO);
    summaryFilterRequest.setCategoryCode(CATEGORY_CODE);
    reviewProductResponses = new ArrayList<>();
    ReviewProductResponse reviewProductResponse = new ReviewProductResponse();
    reviewProductResponse.setCategoryCode(CATEGORY_CODE);
    reviewProductResponse.setProductId(PRODUCT_ID);
    reviewProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    reviewProductResponses.add(reviewProductResponse);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    response = new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID);
    filterCountResponse = new FilterCountResponse();
    filterCountResponse.setAssigned(0);
    filterCountResponse.setUnassigned(100);
    filterCountResponse.setSourceDb(Boolean.FALSE);

    masterAttributeResponse = new MasterAttributeResponse();
    masterAttributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    masterAttributeResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<MasterAttributeResponse>(masterAttributeResponse, REQUEST_ID);

    productHistoryResponse = new ProductHistoryResponse();
    productHistoryResponse.setProductId(PRODUCT_ID);
    productHistoryResponse.setNotes(NOTES);
    productHistoryResponse.setState(1);
    productHistoryResponse.setDescription(HISTORY_DESCRIPTION);

    screeningProductBulkActionsWebRequest =
        ScreeningProductBulkActionsWebRequest.builder().productCodes(Collections.singletonList(PRODUCT_CODE))
            .assignTo(ASSIGNED_TO).assignedBy(ASSIGNED_BY).correctionReason(CORRECTION_REASON)
            .rejectionReason(REJECT_REASON).additionalNotes(ADDITIONAL_NOTES)
            .vendorNotes(Arrays.asList(CORRECTION_REASON)).imageReason(Arrays.asList(CORRECTION_REASON))
            .contentAdditionalNotes(ADDITIONAL_NOTES).imagesAdditionalNotes(ADDITIONAL_NOTES)
            .vendorErrorFields(ERROR_FIELDS).allVariants(Boolean.TRUE).build();
    screeningProductBulkActionsRequest =
        ScreeningProductBulkActionsRequest.builder().productCodes(Collections.singletonList(PRODUCT_CODE))
            .assignTo(ASSIGNED_TO).assignedBy(ASSIGNED_BY).correctionReason(CORRECTION_REASON)
            .rejectionReason(REJECT_REASON).additionalNotes(ADDITIONAL_NOTES).build();
    productSuggestionWebRequests = new ArrayList<>();
    productSuggestionWebRequest = new ProductSuggestionWebRequest();
    productSuggestionWebRequest.setName(BRAND);
    productSuggestionWebRequest.setValue(DEFAULT_BRAND_VALUE);
    productSuggestionWebRequests.add(productSuggestionWebRequest);

    pristineSupportedCategoryMap = new HashMap<>();
    categorySet = new HashSet<>();
    categorySet.add(CATEGORY_ID);
    pristineSupportedCategoryMap.put(CATEGORY, categorySet);

    productCodeResponses = new ArrayList<>();
    productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductCode(PRODUCT_CODE);
    productCodeResponse.setProductName(PRODUCT_NAME);
    productCodeResponses.add(productCodeResponse);

    categoryFinalMap = new HashMap<>();
    categoryFinalMap.put(CATEGORY_ID, CATEGORY);

    attributeReqModel = new AttributeReqModel(BRAND, DEFAULT_BRAND_VALUE);
    categoryDetailResponse = generateCategoryDetailResponse();
    categoryDetailResponse1 = generateCategoryDetailResponse();
    categoryDetailResponse1.setCategoryCode(DEFAULT_CATEGORY_ID_2);
    categoryDetailResponse1.setBopisEligible(false);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(categoryDetailResponse.getCategoryAttributes().get(0).getAttribute());
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    productSuspensionFilterRequest = new ProductSuspensionFilterRequest();
    productSuspensionFilterRequest.setCategoryCode(CATEGORY_CODE);

    summaryFilterRequest1 = SummaryFilterRequest.builder().categoryCode(CATEGORY_CODE).build();

    suspensionProductResponse = new SuspensionProductResponse();
    suspensionProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    suspensionProductResponse.setCategoryCode(CATEGORY_CODE);
    gdnRestListResponse = new GdnRestListResponse<SuspensionProductResponse>();
    gdnRestListResponse.setContent(Arrays.asList(suspensionProductResponse));
    gdnRestListResponse.setSuccess(true);

    productSuspensionHistoryResponse = new ProductSuspensionHistoryResponse();
    productSuspensionHistoryResponse.setProductSku(PRODUCT_SKU);
    productSuspensionHistoryResponse.setReason(DEFAULT_REASON);

    fileContent = new byte[] {-1, -40, -20, -10};
    businessPartnerCodesRequest = new BusinessPartnerCodesRequest();

    merchantNameResponse = new MerchantNameResponse();
    merchantNameResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    merchantNameResponse.setMerchantName(BUSINESS_PARTNER_NAME);
    merchantNameResponseGdnRestListResponse = new GdnRestListResponse<MerchantNameResponse>();
    merchantNameResponseGdnRestListResponse.setContent(Arrays.asList(merchantNameResponse));
    merchantNameResponseGdnRestListResponse.setSuccess(true);

    productItemAttributeValueResponse.setValue(ITEM_ATTRIBUTE_VALUE);
    productItemAttributeValueResponse1.setValue(ITEM_ATTRIBUTE_VALUE_1);
    productItemAttributeValueResponse2.setValue(ITEM_ATTRIBUTE_VALUE);

    attributeResponse.setName(WARNA_ATTRIBUTE_NAME);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse1.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse2.setAttributeResponse(attributeResponse);

    productItemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));
    productItemResponse1.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse1));
    productItemResponse2.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse2));

    productItemResponse.setGeneratedItemName(ITEM_NAME);
    productItemResponse1.setGeneratedItemName(ITEM_NAME_1);
    productItemResponse2.setGeneratedItemName(ITEM_NAME_2);
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse1.setSkuCode(SKU_CODE_1);
    productItemResponse2.setSkuCode(SKU_CODE_2);

    productItemWebResponseSet.add(productItemResponse);
    productItemWebResponseSet.add(productItemResponse1);
    productItemWebResponseSet.add(productItemResponse2);

    productDetailResponseGroupByWarna = new ProductDetailResponse();
    productDetailResponseGroupByWarna.setId(PRODUCT_ID);
    productDetailResponseGroupByWarna.setProductCode(PRODUCT_CODE);
    productDetailResponseGroupByWarna.setProductItemResponses(productItemWebResponseSet);
    responseGroupByWarna = new GdnRestSingleResponse<>(productDetailResponseGroupByWarna, REQUEST_ID);

    summaryFilterWebRequest =
        SummaryFilterWebRequest.builder().activated(true).viewable(true).categoryCode(CATEGORY_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).reviewPending(null).keyword(KEYWORD).sortBy(DEFAULT_SORT_TYPE)
            .build();

    productCollectionResponseGdnRestListResponse = new GdnRestListResponse<>();
    productCollectionResponse = new ProductCollectionResponse();
    productCollectionResponse.setActivated(true);
    productCollectionResponse.setBrand(BRAND);
    productCollectionResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollectionResponse.setCategoryCode(CATEGORY_CODE);
    productCollectionResponse.setProductCode(PRODUCT_CODE);
    productCollectionResponse.setProductName(PRODUCT_NAME);
    productCollectionResponse.setContentApproved(true);
    productCollectionResponse.setImageApproved(true);
    productCollectionResponseGdnRestListResponse.setContent(Arrays.asList(productCollectionResponse));
    productCollectionResponseGdnRestListResponse.setSuccess(true);
    countWebRequest = new CountWebRequest();
    countWebRequest.setBusinessPartnerCode(MERCHANT_CODE);
    countWebRequest.setCategoryCode(CATEGORY_CODE);
    countWebRequest.setKeyword(KEYWORD);

    selectedMasterProductDownloadWebRequest = new SelectedMasterProductDownloadWebRequest();
    selectedMasterProductDownloadWebRequest.setProductCodes(Collections.singletonList(PRODUCT_CODE));

    taskHistoryResponse = new TaskHistoryResponse();
    taskHistoryResponse.setCategoryCode(CATEGORY_CODE);
    taskHistoryResponse.setProductCode(PRODUCT_CODE);
    taskHistoryResponse.setState(STATE);
    taskHistoryResponse.setReason(HISTORY_DESCRIPTION);
    taskHistoryResponse.setVendorCode(BUSINESS_PARTNER_CODE);

    wholesaleMappingResponse.setWholesalePriceConfigEnabled(true);
    wholesaleMappingResponse.setConfigurationType(WHOLESALE_CONFIG_TYPE);

    wholesaleConfigResponse = WholesaleConfigResponse.builder().quantity(QUANTITY)
        .minWholesaleDiscount(Arrays.asList(new MinWholesaleDiscountResponse(null, PERCENTAGE))).build();
    wholesaleMappingResponse.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse));

    wholesaleMappingResponse2.setWholesalePriceConfigEnabled(true);
    wholesaleMappingResponse2.setConfigurationType(WHOLESALE_CONFIG_TYPE);

    wholesaleConfigResponse = WholesaleConfigResponse.builder().quantity(QUANTITY_2)
        .minWholesaleDiscount(Arrays.asList(new MinWholesaleDiscountResponse(null, PERCENTAGE_2))).build();
    wholesaleMappingResponse2.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse));

    CategoryNamesResponse categoryNamesResponse = new CategoryNamesResponse();
    Map<String, String> categoryMap = new HashMap<>();
    categoryMap.put(CATEGORY_CODE, CATEGORY_NAME);
    categoryNamesResponse.setCategoryMap(categoryMap);
    categoryNamesResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(categoryNamesResponse, REQUEST_ID);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setMerchantType(CC);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);
    profileResponse.setCompany(companyDTO);
    List<ProfileResponse> profileResponseList = new ArrayList<>();
    profileResponseList.add(profileResponse);
    ProfileResponse profileResponse2 = new ProfileResponse();
    profileResponse2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setInternationalFlag(true);
    companyDTO1.setMerchantType(CM);
    profileResponse2.setCompany(companyDTO1);
    profileResponseList.add(profileResponse2);
    profileResponseGdnRestListResponse = new GdnRestListResponse<>(profileResponseList, new PageMetaData(), REQUEST_ID);
    profileResponseMap = profileResponseGdnRestListResponse.getContent().stream()
        .collect(Collectors.toMap(profileResponse1 -> profileResponse1.getBusinessPartnerCode(), Function.identity()));
    marginCategoryResponse.setValue(1000.0);

    halalProductHistoryResponse = new HalalProductHistoryResponse();
    halalProductHistoryResponse.setProductSku(PRODUCT_SKU);
    halalProductHistoryResponse.setPreviousValue(PREVIOUS_VALUE);
    halalProductHistoryResponse.setCurrentValue(CURRENT_VALUE);
    halalProductHistoryResponse.setActivity(ACTIVITY);

    halalProductHistoryWebResponse = new HalalProductHistoryWebResponse();
    halalProductHistoryWebResponse.setProductSku(PRODUCT_SKU);
    halalProductHistoryWebResponse.setActivity(ACTIVITY);
    halalProductHistoryWebResponse.setCurrentValue(CURRENT_VALUE);
    halalProductHistoryWebResponse.setPreviousValue(PREVIOUS_VALUE);
    halalProductHistoryWebResponse.setCreatedBy(CREATED_BY);
    halalProductHistoryWebResponse.setUpdatedBy(CREATED_BY);
    ReflectionTestUtils.setField(productService, "halalThirdPartyApiSwitch", false);
    ReflectionTestUtils.setField(productService, "productDetailPageUrlPrefix", "https://www.blibli.com/p/product-detail/pc--");
    Mockito.when(clientParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);

    productAttributeRequestDSExtractedPredefined.setAttribute(
        AttributeRequest.builder().attributeCode(ATTRIBUTE_CODE).dsExtraction(true)
            .attributeType(AttributeType.PREDEFINED_ATTRIBUTE).build());
    predefinedAllowedAttributeValueRequestDSExtracted.setId(ID1);
    predefinedAllowedAttributeValueRequestDSExtracted.setValue(DS_EXTRACTED_ATTRIBUTE_VALUE1);
    productAttributeValueRequestDSExtractedPredefined.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueRequestDSExtracted);
    productAttributeRequestDSExtractedPredefined.setProductAttributeValues(
        Collections.singletonList(productAttributeValueRequestDSExtractedPredefined));

    predefinedAllowedAttributeValueResponseDSExtracted.setValue(DS_EXTRACTED_ATTRIBUTE_VALUE1);
    predefinedAllowedAttributeValueResponseDSExtracted.setId(ID1);
    productAttributeValueResponseDSExtractedPredefined.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueResponseDSExtracted);
    productAttributeResponseDSExtractedPredefined.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponseDSExtractedPredefined));
    productAttributeResponseDSExtractedPredefined.setAttribute(
        AttributeResponse.builder().attributeCode(ATTRIBUTE_CODE).dsExtraction(true)
            .attributeType(AttributeType.PREDEFINED_ATTRIBUTE.name()).build());

    productAttributeValueRequestDSExtractedDescriptive.setDescriptiveAttributeValue(
        DS_EXTRACTED_ATTRIBUTE_VALUE1);
    productAttributeRequestDSExtractedDescriptive.setProductAttributeValues(
        Collections.singletonList(productAttributeValueRequestDSExtractedDescriptive));
    productAttributeRequestDSExtractedDescriptive.setAttribute(
        AttributeRequest.builder().attributeCode(ATTRIBUTE_CODE).dsExtraction(true)
            .attributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE).build());

    productAttributeValueResponseDSExtractedDescriptive.setDescriptiveAttributeValue(
        DS_EXTRACTED_ATTRIBUTE_VALUE2);
    productAttributeResponseDSExtractedDescriptive.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponseDSExtractedDescriptive));
    productAttributeResponseDSExtractedDescriptive.setAttribute(
        AttributeResponse.builder().attributeCode(ATTRIBUTE_CODE).dsExtraction(true)
            .attributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name()).build());

    predefinedAllowedAttributeValueRequestDSExtracted2.setId(ID2);
    predefinedAllowedAttributeValueRequestDSExtracted2.setValue(DS_EXTRACTED_ATTRIBUTE_VALUE2);
    productAttributeValueRequestDSExtractedPredefined2.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueRequestDSExtracted2);
    productAttributeRequestDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequestDSExtractedPredefined,
            productAttributeValueRequestDSExtractedPredefined2));
    productAttributeRequestDSExtractedPredefinedMultiValue.setAttribute(
        AttributeRequest.builder().attributeCode(ATTRIBUTE_CODE)
            .attributeType(AttributeType.PREDEFINED_MULTIVALUE).dsExtraction(true).build());

    productAttributeResponseDSExtractedPredefinedMultiValue.setAttribute(
        AttributeResponse.builder().attributeCode(ATTRIBUTE_CODE)
            .attributeType(AttributeType.PREDEFINED_MULTIVALUE.name()).dsExtraction(true).build());
    predefinedAllowedAttributeValueResponseDSExtracted2.setValue(DS_EXTRACTED_ATTRIBUTE_VALUE2);
    predefinedAllowedAttributeValueResponseDSExtracted2.setId(ID2);
    productAttributeValueResponseDSExtractedPredefined2.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueResponseDSExtracted2);
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueResponseDSExtractedPredefined,
            productAttributeValueResponseDSExtractedPredefined2));

    productAttributeValueRequestDSExtractedDescriptive2.setDescriptiveAttributeValue(
        DS_EXTRACTED_ATTRIBUTE_VALUE2);
    productAttributeValueResponseDSExtractedDescriptive2.setDescriptiveAttributeValue(
        DS_EXTRACTED_ATTRIBUTE_VALUE1);
    productAttributeRequestDSExtractedDescriptiveMultiValue.setAttribute(
        AttributeRequest.builder().attributeCode(ATTRIBUTE_CODE)
            .attributeType(AttributeType.DESCRIPTIVE_MULTIVALUE).dsExtraction(true).build());
    productAttributeRequestDSExtractedDescriptiveMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequestDSExtractedDescriptive,
            productAttributeValueRequestDSExtractedDescriptive2));
    productAttributeResponseDSExtractedDescriptiveMultiValue.setAttribute(
        AttributeResponse.builder().attributeCode(ATTRIBUTE_CODE).dsExtraction(true)
            .attributeType(AttributeType.DESCRIPTIVE_MULTIVALUE.name()).build());

    productAttributeResponseDSExtractedDescriptiveMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueResponseDSExtractedDescriptive,
            productAttributeValueResponseDSExtractedDescriptive2));
  }

  private CategoryDetailResponse generateCategoryDetailResponse() {
    List<CategoryReferenceResponse> categoryReferenceResponses = new ArrayList<CategoryReferenceResponse>();
    CategoryReferenceResponse categoryReferenceResponse = new CategoryReferenceResponse();
    categoryReferenceResponse.setId(DEFAULT_CATEGORY_ID);
    categoryReferenceResponse.setMasterCategoryReference(new CategoryResponse());
    categoryReferenceResponse.getMasterCategoryReference().setId(DEFAULT_CATEGORY_ID);
    categoryReferenceResponses.add(categoryReferenceResponse);

    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<CategoryAttributeResponse>();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(new AttributeResponse());
    categoryAttributeResponse.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    categoryAttributeResponse.getAttribute().setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    categoryAttributeResponse.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    categoryAttributeResponse.getAttribute().setMarkForDelete(false);
    categoryAttributeResponse.getAttribute().setName(DEFAULT_NAME);
    categoryAttributeResponse.getAttribute().setVariantCreation(true);
    categoryAttributeResponse.setId(DEFAULT_CATEGORY_ID);
    categoryAttributeResponse.setMarkForDelete(false);
    categoryAttributeResponses.add(categoryAttributeResponse);

    CategoryDetailResponse response = new CategoryDetailResponse();
    response.setMasterCategoryReferences(categoryReferenceResponses);
    response.setCatalog(new CatalogResponse());
    response.setCategoryAttributes(categoryAttributeResponses);
    response.setCategoryCode(DEFAULT_CATEGORY_CODE);
    response.setParentCategoryId(DEFAULT_CATEGORY_ID_2);
    response.setCreatedBy(DEFAULT_CREATED_BY);
    response.setCategoryCode(DEFAULT_CATEGORY_CODE);
    response.setCreatedDate(new Date());
    response.setDefaultDescription(new byte[1]);
    response.setDescription(new byte[1]);
    response.setDisplay(true);
    response.setId(DEFAULT_CATEGORY_ID);
    response.setLogisticAdjustment(DEFAULT_LOGISTIC_ADJUSTMENT);
    response.setMarkForDelete(false);
    response.setName(DEFAULT_NAME);
    response.setNeedIdentity(false);
    response.setParentCategoryId(DEFAULT_CATEGORY_ID_2);
    response.setSalesCategoryReferences(new ArrayList<CategoryReferenceResponse>());
    response.getSalesCategoryReferences().add(new CategoryReferenceResponse());
    response.setSequence(DEFAULT_SEQUENCE);
    response.setShortDescription(DEFAULT_DESCRIPTION);
    response.setState(DEFAULT_STATE);
    response.setStoreId(DEFAULT_STORE_ID);
    response.setUpdatedBy(DEFAULT_UPDATED_BY);
    response.setUpdatedDate(new Date());
    response.setVersion(DEFAULT_VERSION);
    response.setViewable(false);
    response.setWarranty(false);
    response.setBopisEligible(true);
    return response;
  }


  @Test
  public void returnForCorrectionTest() {
    productService.returnForCorrection(productReturnForCorrectionRequest);
    Mockito.verify(this.productWorkflowService).returnForCorrection(productReturnForCorrectionRequest);
  }

  @Test
  public void mergeProductTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.mergeProducts(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false)).thenReturn(response);
    productService.mergeProduct(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false);
    Mockito.verify(this.pbpFeign).mergeProducts(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false);
  }

  @Test
  public void mergeProduct_WhenClientExceptionTest() {
    boolean isResponseInvalid = true;
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, null);
    Mockito.when(pbpFeign.mergeProducts(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false)).thenReturn(response);
    try {
      productService.mergeProduct(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false);
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(this.pbpFeign).mergeProducts(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false);
      Assertions.assertFalse(isResponseInvalid);
    }
  }

  @Test
  public void testUpdateProductAndPublishToPDT() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateAndPublishProductToPDT(productRequest)).thenReturn(response);
    this.productService.updateProductAndPublishToPDT(productRequest);
    Mockito.verify(pbpFeign).updateAndPublishProductToPDT(productRequest);
  }

  @Test
  public void testUpdateProductAndPublishToPDT_whenClientException() {
    boolean isResponseInvalid = true;
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, null);
    Mockito.when(pbpFeign.updateAndPublishProductToPDT(productRequest)).thenReturn(response);
    try {
      this.productService.updateProductAndPublishToPDT(productRequest);
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(pbpFeign).updateAndPublishProductToPDT(productRequest);
      Assertions.assertFalse(isResponseInvalid);
    }
  }

  @Test
  public void findProductTest() {
    ProductDetailResponse response = null;
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    GdnRestSingleResponse<ProductDetailResponse> clientResponse =
        new GdnRestSingleResponse<>(null, null, true, productDetailResponse, null);
    Mockito.when(pbpFeign.getProduct(PRODUCT_ID)).thenReturn(clientResponse);
    response = this.productService.findProduct(PRODUCT_ID);
    Mockito.verify(pbpFeign).getProduct(PRODUCT_ID);
    assertEquals(productDetailResponse, response);
  }

  @Test
  public void findProductTest_whenClientException() {
    ProductDetailResponse response = null;
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    boolean isResponseInvalid = true;
    GdnRestSingleResponse<ProductDetailResponse> clientResponse =
        new GdnRestSingleResponse<>(null, null, false, productDetailResponse, null);
    Mockito.when(pbpFeign.getProduct(PRODUCT_ID)).thenReturn(clientResponse);
    try {
      response = this.productService.findProduct(PRODUCT_ID);
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(pbpFeign).getProduct(PRODUCT_ID);
      Assertions.assertFalse(isResponseInvalid);
      Assertions.assertNull(response);
    }
  }

  @Test
  public void generateBarCodeTest() {
    String response = null;
    GdnRestSimpleResponse<String> clientResponse = new GdnRestSimpleResponse<>(null, null, true, null, BARCODE);
    Mockito.when(pbpFeign.generateBarcode()).thenReturn(clientResponse);
    response = this.productService.generateBarcode();
    Mockito.verify(pbpFeign).generateBarcode();
    assertEquals(BARCODE, response);

  }

  @Test
  public void generateBarCodeTest_whenClientException() {
    boolean isResponseInvalid = true;
    String response = null;
    GdnRestSimpleResponse<String> clientResponse = new GdnRestSimpleResponse<>(null, null, false, null, BARCODE);
    Mockito.when(pbpFeign.generateBarcode()).thenReturn(clientResponse);
    try {
      response = this.productService.generateBarcode();
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(pbpFeign).generateBarcode();
      Assertions.assertFalse(isResponseInvalid);
      Assertions.assertNull(response);
    }
  }

  @Test
  public void updateProductTest() throws Exception {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void updateProductTest_whenClientException() throws Exception {
    boolean isResponseInvalid = true;
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    try {
      this.productService.updateProduct(productRequest, false);
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(pbpFeign).updateProduct(productRequest);
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
      Assertions.assertFalse(isResponseInvalid);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateNotInScreeningProductTest() throws Exception {
    productDetailResponse.setActivated(true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    try {
      this.productService.updateProduct(productRequest, false);
    } catch (InvalidStateException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedAttributeUpdated() throws Exception {
    predefinedAllowedAttributeValueRequestDSExtracted.setValue(DS_EXTRACTED_ATTRIBUTE_VALUE2);
    productAttributeValueRequestDSExtractedPredefined.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueRequestDSExtracted);
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefined));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefined));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedAttributeNotPresentDueToCategoryChange()
      throws Exception {
    productAttributeRequestDSExtractedDescriptive.getAttribute()
        .setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptive));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefined));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedAttributeNotUpdated() throws Exception {
    productAttributeRequestDSExtractedPredefined.setProductAttributeValues(
        Collections.singletonList(productAttributeValueRequestDSExtractedPredefined));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefined));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefined));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveAttributeUpdated() throws Exception {
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptive));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptive));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveAttributeNotUpdated() throws Exception {
    productAttributeValueRequestDSExtractedDescriptive.setDescriptiveAttributeValue(
        DS_EXTRACTED_ATTRIBUTE_VALUE2);
    productAttributeRequestDSExtractedDescriptive.setProductAttributeValues(
        Collections.singletonList(productAttributeValueRequestDSExtractedDescriptive));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptive));
    ;
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptive));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueNotUpdated() throws Exception {
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueNotUpdatedOrderChanged()
      throws Exception {
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueResponseDSExtractedPredefined2,
            productAttributeValueResponseDSExtractedPredefined));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueUpdated() throws Exception {
    productAttributeValueResponseDSExtractedPredefined.setPredefinedAllowedAttributeValue(
        predefinedAllowedAttributeValueResponseDSExtracted2);
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueResponseDSExtractedPredefined,
            productAttributeValueResponseDSExtractedPredefined2));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueNewValuesAdded() throws Exception {
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponseDSExtractedPredefined));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueAllValuesRemoved() throws Exception {
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Collections.singletonList(new ProductAttributeValueResponse()));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedPredefinedMultiValueInitiallyNoValuesPresentAndNewValuesAdded() throws Exception {
    productAttributeResponseDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Collections.singletonList(productAttributeValueResponseDSExtractedPredefined));
    productAttributeRequestDSExtractedPredefinedMultiValue.setProductAttributeValues(
        Collections.singletonList(new ProductAttributeValueRequest()));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedPredefinedMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedPredefinedMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveMultiValueNotUpdated() throws Exception {
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptiveMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptiveMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveMultiValueNotUpdatedOrderChanged()
      throws Exception {
    productAttributeRequestDSExtractedDescriptiveMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequestDSExtractedDescriptive2,
            productAttributeValueRequestDSExtractedDescriptive));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptiveMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptiveMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verifyNoInteractions(kafkaProducer);
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveMultiValueUpdated() throws Exception {
    productAttributeValueRequestDSExtractedDescriptive.setDescriptiveAttributeValue("3");
    productAttributeRequestDSExtractedDescriptiveMultiValue.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequestDSExtractedDescriptive,
            productAttributeValueRequestDSExtractedDescriptive2));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptiveMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptiveMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void updateProductTest_dsExtractedDescriptiveMultiValueRemoved() throws Exception {
    productAttributeRequestDSExtractedDescriptiveMultiValue.setProductAttributeValues(
        Collections.singletonList(productAttributeValueRequestDSExtractedDescriptive));
    productRequest.setProductAttributes(
        Collections.singletonList(productAttributeRequestDSExtractedDescriptiveMultiValue));
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setProductAttributeResponses(
        Collections.singletonList(productAttributeResponseDSExtractedDescriptiveMultiValue));
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(response);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    Mockito.when(kafkaTopicProperties.getProductAttributeFeedbackEventName())
        .thenReturn(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME);
    this.productService.updateProduct(productRequest, false);
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(kafkaProducer)
        .send(eq(PRODUCT_ATTRIBUTE_EXTRACTIONS_FEEDBACK_EVENT_NAME), eq(PRODUCT_CODE),
            any(ProductAttributeFeedbackEventModel.class));
  }

  @Test
  public void getReviewProductsByFilterRequestTest() {
    when(pbpFeign
        .getReviewProducts(any(SummaryFilterRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE)))
        .thenReturn(new GdnRestListResponse<>(reviewProductResponses, new PageMetaData(), REQUEST_ID));
    when(bpService.getProfileResponseMap(Mockito.anyList())).thenReturn(profileResponseMap);
    Page<ReviewProductWebResponse> response = productService
        .getReviewProductsByFilterRequest(reviewProductsFilterRequest, Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    verify(pbpFeign)
        .getReviewProducts(summaryFilterRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE),
            eq(SIZE));
    verify(bpService).getProfileResponseMap(Mockito.anyList());
    assertEquals(PRODUCT_ID, response.getContent().get(0).getProductId());
    assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    assertEquals(CC, response.getContent().get(0).getCommissionType());
  }

  @Test
  public void getReviewProductsByFilterRequestExceptionTest() {
    when(pbpFeign
        .getReviewProducts(any(SummaryFilterRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE)))
        .thenReturn(null);
    try {
      productService
          .getReviewProductsByFilterRequest(reviewProductsFilterRequest, Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    } catch (ClientException ex) {
    } finally {
      verify(pbpFeign)
          .getReviewProducts(summaryFilterRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE),
              eq(PAGE), eq(SIZE));
    }
  }

  @Test
  public void updateProductAssignmentStatusTest() {
    Mockito.when(pbpFeign.productAssignment(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.updateProductAssignmentStatus(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    Mockito.verify(pbpFeign).productAssignment(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
  }

  @Test
  public void updateProductAssignmentStatusExceptionTest() {
    Mockito.when(pbpFeign.productAssignment(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY)).thenReturn(null);
    try {
      productService.updateProductAssignmentStatus(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pbpFeign).productAssignment(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    }
  }

  @Test
  public void getProductDetailTest() {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, false);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertEquals(DETAIL_PAGE_URL, response.getProductDetailPageLink());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductDetailNeedRevisionAppExceptionPostLiveProductTest() {
    productDetailResponse.setRevised(true);
    productDetailResponse.setPostLive(true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    try {
      ProductDetailWebResponse response =
          productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, Constants.MTA_APP, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductDetailNeedRevisionAppExceptionEditedProductTest() {
    productDetailResponse.setRevised(true);
    productDetailResponse.setEdited(true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    try {
      ProductDetailWebResponse response =
          productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, Constants.MTA_APP, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductDetailNeedRevisionAppExceptionRevisedProductTest() {
    productDetailResponse.setRevised(true);
    productDetailResponse.setEdited(true);
    productDetailResponse.setPostLive(true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    try {
      ProductDetailWebResponse response =
          productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, Constants.MTA_APP, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductDetailXBPExceptionTest() {
    productDetailResponse.setRevised(false);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1)).thenReturn(null);
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, Constants.MTA_APP, false);

    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertNull(response.getCommissionType());
  }

  @Test
  public void getProductDetailXBPExceptionNotEditedPostliveTest() {
    ReflectionTestUtils.setField(productService, "valueTypeAdditionForDefiningAttributes", false);
    productDetailResponse.setRevised(true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1)).thenReturn(null);
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, Constants.MTA_APP, true);

    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertNull(response.getCommissionType());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductDetailExceptionTest() {
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE)).thenReturn(null);
    try {
      productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, false);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductDetailTest_groupByWarnaTest() {
    ReflectionTestUtils.setField(productService, "valueTypeAdditionForDefiningAttributes", true);
    productDetailResponseGroupByWarna = new ProductDetailResponse();
    productDetailResponseGroupByWarna.setId(PRODUCT_ID);
    productDetailResponseGroupByWarna.setProductCode(PRODUCT_CODE);
    productDetailResponseGroupByWarna.setProductItemResponses(productItemWebResponseSet);
    BeanUtils.copyProperties(productDetailResponseGroupByWarna, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);

    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, true);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService)
        .getProfileResponseByBusinessPartnerCode(responseGroupByWarna.getValue().getBusinessPartnerCode());
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    Iterator<ProductItemWebResponse> iter = response.getProductItemResponses().iterator();

    assertEquals(ITEM_NAME, iter.next().getGeneratedItemName());
    assertEquals(ITEM_NAME_2, iter.next().getGeneratedItemName());
    assertEquals(ITEM_NAME_1, iter.next().getGeneratedItemName());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductDetailTest_instoreEnabledTruePureInstoreProductTest() {
    ReflectionTestUtils.setField(productService, "instoreNewFlowEnabled", true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(
        new GdnRestSingleResponse<>(
            ProductL3BasicResponse.builder().pureInStoreProduct(true).build(), REQUEST_ID));
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, false);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertTrue(response.isPureInstore());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductDetailTest_instoreEnabledTrueNonInstoreSellerTest() {
    ReflectionTestUtils.setField(productService, "instoreNewFlowEnabled", true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, false);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertFalse(response.isPureInstore());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getProductDetailTest_instoreEnabledTrueNullPBPResponseTest() {
    ReflectionTestUtils.setField(productService, "instoreNewFlowEnabled", true);
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productCompleteResponseGdnRestSingleResponse = new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID);
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE))
        .thenReturn(productCompleteResponseGdnRestSingleResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(
        new GdnRestSingleResponse<>(
            null, REQUEST_ID));
    ProductDetailWebResponse response = productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, clientId, false);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    assertEquals(PRODUCT_ID, response.getId());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertFalse(response.isPureInstore());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void getFilterCountsTest() {
    Mockito.when(pbpFeign.getFilterCounts(Boolean.FALSE, Boolean.FALSE))
        .thenReturn(new GdnRestSingleResponse<>(filterCountResponse, REQUEST_ID));
    FilterCountWebResponse response = productService.getFilterCounts(Boolean.FALSE, Boolean.FALSE);
    Mockito.verify(pbpFeign).getFilterCounts(Boolean.FALSE, Boolean.FALSE);
    assertEquals(0, response.getAssigned());
    assertEquals(100, response.getUnassigned());
    assertEquals(Boolean.FALSE, response.isSourceDb());
  }

  @Test
  public void getFilterExceptionTest() {
    Mockito.when(pbpFeign.getFilterCounts(Boolean.FALSE, Boolean.FALSE)).thenReturn(null);
    try {
      productService.getFilterCounts(Boolean.FALSE, Boolean.FALSE);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pbpFeign).getFilterCounts(Boolean.FALSE, Boolean.FALSE);
    }
  }

  @Test
  public void approveDraftTest() throws Exception {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(pbpFeign.approveDraft(productRequest.getProductCode())).thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    productService.approveDraft(productRequest);
    Mockito.verify(pbpFeign).approveDraft(productRequest.getProductCode());
    Mockito.verify(pbpFeign).updateProduct(productRequest);
    Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void approveDraftExceptionTest() throws Exception {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    Mockito.when(pbpFeign.updateProduct(productRequest)).thenReturn(new GdnBaseRestResponse(null, null, true, null));
    Mockito.when(pbpFeign.approveDraft(productRequest.getProductCode())).thenReturn(null);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    try {
      productService.approveDraft(productRequest);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pbpFeign).approveDraft(productRequest.getProductCode());
      Mockito.verify(pbpFeign).updateProduct(productRequest);
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void approveNotInDraftStateProductTest() throws Exception {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setActivated(true);
    Mockito.when(pbpFeign.getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse, REQUEST_ID));
    try {
      productService.approveDraft(productRequest);
    } catch (InvalidStateException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pbpFeign).getProductDetail(PRODUCT_CODE, false, BUSINESS_PARTNER_CODE);
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    }
  }

  @Test
  public void getProductHistoryTest() {
    when(pbpFeign.getProductHistory(PRODUCT_ID, PAGE, SIZE))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(productHistoryResponse), new PageMetaData(), REQUEST_ID));
    Page<ProductHistoryWebResponse> response = productService.getProductHistory(PRODUCT_ID, PAGE, SIZE);
    verify(pbpFeign).getProductHistory(PRODUCT_ID, PAGE, SIZE);
    assertEquals(PRODUCT_ID, response.getContent().get(0).getProductId());
    assertEquals(NOTES_ASSERTION, response.getContent().get(0).getNotes().get(0));
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE, response.getContent().get(0).getDescription());
  }

  @Test
  public void getProductHistoryExceptionTest() {
    when(pbpFeign.getProductHistory(PRODUCT_ID, PAGE, SIZE)).thenReturn(null);
    try {
      productService.getProductHistory(PRODUCT_ID, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).getProductHistory(PRODUCT_ID, PAGE, SIZE);
    }
  }

  @Test
  public void doScreeningProductsBulkActionsTest() {
    when(pbpFeign.doScreeningProductsBulkActions(eq(BULK_ACTION),
        screeningProductBulkActionsRequestArgumentCaptor.capture())).thenReturn(new GdnBaseRestResponse(REQUEST_ID));
    productService.doScreeningProductsBulkActions(BULK_ACTION, screeningProductBulkActionsWebRequest);
    verify(pbpFeign).doScreeningProductsBulkActions(eq(BULK_ACTION),
        eq(screeningProductBulkActionsRequestArgumentCaptor.getValue()));
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(CORRECTION_REASON,
        screeningProductBulkActionsRequestArgumentCaptor.getValue().getVendorNotes().get(0));
    Assertions.assertEquals(CORRECTION_REASON,
        screeningProductBulkActionsRequestArgumentCaptor.getValue().getImageReason().get(0));
    Assertions.assertEquals(ADDITIONAL_NOTES,
        screeningProductBulkActionsRequestArgumentCaptor.getValue().getContentAdditionalNotes());
    Assertions.assertEquals(ADDITIONAL_NOTES,
        screeningProductBulkActionsRequestArgumentCaptor.getValue().getImagesAdditionalNotes());
    Assertions.assertEquals(ERROR_FIELDS.get(0),
        screeningProductBulkActionsRequestArgumentCaptor.getValue().getVendorErrorFields().get(0));
    Assertions.assertTrue(screeningProductBulkActionsRequestArgumentCaptor.getValue().getAllVariants());
  }

  @Test
  public void doScreeningProductsBulkActionsExceptionTest() {
    when(pbpFeign.doScreeningProductsBulkActions(eq(BULK_ACTION), screeningProductBulkActionsRequestArgumentCaptor.capture()))
        .thenReturn(null);
    try {
      productService.doScreeningProductsBulkActions(BULK_ACTION, screeningProductBulkActionsWebRequest);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).doScreeningProductsBulkActions(eq(BULK_ACTION), screeningProductBulkActionsRequestArgumentCaptor.capture());
    }
  }

  @Test
  public void getScreeningSuggestionPristineSupportedCategoryTest() {
    when(extCatalogProperties.getNeedPristineSuggestion()).thenReturn(String.valueOf(true));
    when(extCatalogService.getSupportedBlibliCategoriesByPristine()).thenReturn(pristineSupportedCategoryMap);
    when(extCatalogService.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PageRequest.of(PAGE, SIZE)))
        .thenReturn(productCodeResponses);
    List<ProductSuggestionWebResponse> response = productService
        .getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID, productSuggestionWebRequests,
            PageRequest.of(PAGE, SIZE));
    verify(extCatalogProperties).getNeedPristineSuggestion();
    verify(extCatalogService).getSupportedBlibliCategoriesByPristine();
    verify(extCatalogService).getPCBProductCodes(PRODUCT_CODE, CATEGORY, PageRequest.of(PAGE, SIZE));
    assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void getScreeningSuggestionTest() {
    when(extCatalogProperties.getNeedPristineSuggestion()).thenReturn(String.valueOf(false));
    when(categoryService.getCategoryToFinalParentMap()).thenReturn(categoryFinalMap);
    when(pbpFeign.findByNameOrUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productCodeResponse), new PageMetaData(PAGE, SIZE, 1), REQUEST_ID));
    List<ProductSuggestionWebResponse> response = productService
        .getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID, productSuggestionWebRequests,
            PageRequest.of(PAGE, SIZE));
    verify(extCatalogProperties).getNeedPristineSuggestion();
    verify(categoryService).getCategoryToFinalParentMap();
    verify(pbpFeign)
        .findByNameOrUpcCode(eq(PRODUCT_NAME), eq(UPC_CODE), eq(CATEGORY), attributeReqModels.capture(), eq(PAGE),
            eq(SIZE));
    assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void getScreeningSuggestionWithUpdatedCategoryMapTest() {
    when(extCatalogProperties.getNeedPristineSuggestion()).thenReturn(String.valueOf(false));
    when(categoryService.getCategoryToFinalParentMap()).thenReturn(null);
    when(pbpFeign.findByNameOrUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productCodeResponse), new PageMetaData(PAGE, SIZE, 1), REQUEST_ID));
    when(categoryService.getFinalParentCategoryAndUpdateMap(CATEGORY_ID)).thenReturn(CATEGORY);
    List<ProductSuggestionWebResponse> response = productService
        .getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID, productSuggestionWebRequests,
            PageRequest.of(PAGE, SIZE));
    verify(extCatalogProperties).getNeedPristineSuggestion();
    verify(categoryService).getCategoryToFinalParentMap();
    verify(pbpFeign)
        .findByNameOrUpcCode(eq(PRODUCT_NAME), eq(UPC_CODE), eq(CATEGORY), attributeReqModels.capture(), eq(PAGE),
            eq(SIZE));
    verify(categoryService).getFinalParentCategoryAndUpdateMap(CATEGORY_ID);
    assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void getScreeningSuggestionClientExceptionTest() {
    when(extCatalogProperties.getNeedPristineSuggestion()).thenReturn(String.valueOf(false));
    when(categoryService.getCategoryToFinalParentMap()).thenReturn(categoryFinalMap);
    when(pbpFeign.findByNameOrUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(null);
    try {
      productService
          .getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID, productSuggestionWebRequests,
              PageRequest.of(PAGE, SIZE));
    } catch (ClientException e) {
    } finally {
      verify(extCatalogProperties).getNeedPristineSuggestion();
      verify(categoryService).getCategoryToFinalParentMap();
      verify(pbpFeign)
          .findByNameOrUpcCode(eq(PRODUCT_NAME), eq(UPC_CODE), eq(CATEGORY), attributeReqModels.capture(), eq(PAGE),
              eq(SIZE));
    }
  }

  @Test
  public void productScreeningSearchTest() {
    when(pbpFeign.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(productCodeResponse), new PageMetaData(PAGE, SIZE, 1), REQUEST_ID));
    List<ProductSuggestionWebResponse> response = productService.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
    verify(pbpFeign).filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
    assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void productScreeningSearchClientExceptionTest() {
    when(pbpFeign.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE)).thenReturn(null);
    try {
      productService.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
    }
  }

  @Test
  public void getProductReviewerListTest() {
    when(cacheProductService.getReviewerList(PRODUCT_CODE)).thenReturn(Arrays.asList(Constants.USER_NAME));
    ProductReviewerWebResponse response =
        productService.getProductReviewerList(PRODUCT_CODE, USER_NAME_2);
    verify(cacheProductService).getReviewerList(PRODUCT_CODE);
    verify(cacheProductService).addUserToProductReviewList(PRODUCT_CODE, USER_NAME_2);
    assertEquals(Constants.USER_NAME, response.getUsername().get(0));
  }

  @Test
  public void getProductReviewerListWhenSameUserTest() {
    when(cacheProductService.getReviewerList(PRODUCT_CODE)).thenReturn(Collections.singletonList(Constants.USER_NAME));
    ProductReviewerWebResponse response =
        productService.getProductReviewerList(PRODUCT_CODE, Constants.USER_NAME);
    verify(cacheProductService).getReviewerList(PRODUCT_CODE);
    verify(cacheProductService).addUserToProductReviewList(PRODUCT_CODE, Constants.USER_NAME);
    assertTrue(CollectionUtils.isEmpty(response.getUsername()));
  }

  @Test
  public void deleteProductReviewerTest() {
    productService.deleteProductReviewer(PRODUCT_CODE, Constants.USER_NAME);
    verify(cacheProductService).removeCurrentUserFromProductView(PRODUCT_CODE, Constants.USER_NAME);
  }

  @Test
  public void getProductRevisionHistoryTest() {
    ItemNeedRevisionNotes itemNeedRevisionNotes = ItemNeedRevisionNotes.builder().itemName(ITEM_NAME).itemNumber(1)
        .vendorErrorFields(Arrays.asList(REJECT_REASON, REQUEST_ID)).skuCode(ITEM_NAME).build();
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ProductRevisionInfoResponse.builder().additionalNotes(ADDITIONAL_NOTES).allVariants(true)
            .contentAdditionalNotes(ADDITIONAL_NOTES).correctionReason(CORRECTION_REASON)
            .imageReason(Collections.singletonList(REQUEST_ID)).vendorNotes(Arrays.asList(REQUEST_ID, ADDITIONAL_NOTES))
            .vendorErrorFields(Arrays.asList(REQUEST_ID, ADDITIONAL_NOTES)).imagesAdditionalNotes(ADDITIONAL_NOTES)
            .itemNotes(Arrays.asList(itemNeedRevisionNotes)).build();
    productRevisionInfoResponse.setId(ID);
    productRevisionInfoResponse.setStoreId(STORE_ID);
    productRevisionInfoResponse.setCreatedBy(CREATED_BY);
    productRevisionInfoResponse.setCreatedDate(CREATED_DATE);
    productRevisionInfoResponse.setUpdatedBy(UPDATED_BY);
    productRevisionInfoResponse.setUpdatedDate(UPDATED_DATE);
    when(pbpFeign.getProductRevisionHistory(PRODUCT_CODE)).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(productRevisionInfoResponse),
            new PageMetaData(PAGE, SIZE, 1), REQUEST_ID));
    List<ProductRevisionHistoryWebResponse> response = productService.getProductRevisionHistory(PRODUCT_CODE);
    verify(pbpFeign).getProductRevisionHistory(PRODUCT_CODE);
    assertEquals(CORRECTION_REASON, response.get(0).getCorrectionReason());
    assertEquals(ADDITIONAL_NOTES, response.get(0).getAdditionalNotes());
    assertEquals(CREATED_BY, response.get(0).getCreatedBy());
    assertEquals(CREATED_DATE, response.get(0).getCreatedDate());
  }

  @Test
  public void getProductRevisionHistoryTest_Empty() {
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ProductRevisionInfoResponse.builder().additionalNotes(ADDITIONAL_NOTES).allVariants(true)
            .contentAdditionalNotes(ADDITIONAL_NOTES).correctionReason(CORRECTION_REASON)
            .imageReason(Collections.singletonList(REQUEST_ID)).vendorNotes(Arrays.asList(REQUEST_ID, ADDITIONAL_NOTES))
            .vendorErrorFields(Arrays.asList(REQUEST_ID, ADDITIONAL_NOTES)).imagesAdditionalNotes(ADDITIONAL_NOTES)
            .itemNotes(null).build();
    productRevisionInfoResponse.setId(ID);
    productRevisionInfoResponse.setStoreId(STORE_ID);
    productRevisionInfoResponse.setCreatedBy(CREATED_BY);
    productRevisionInfoResponse.setCreatedDate(CREATED_DATE);
    productRevisionInfoResponse.setUpdatedBy(UPDATED_BY);
    productRevisionInfoResponse.setUpdatedDate(UPDATED_DATE);
    when(pbpFeign.getProductRevisionHistory(PRODUCT_CODE)).thenReturn(
        new GdnRestListResponse<>(Collections.singletonList(productRevisionInfoResponse),
            new PageMetaData(PAGE, SIZE, 1), REQUEST_ID));
    List<ProductRevisionHistoryWebResponse> response = productService.getProductRevisionHistory(PRODUCT_CODE);
    verify(pbpFeign).getProductRevisionHistory(PRODUCT_CODE);
    assertEquals(CORRECTION_REASON, response.get(0).getCorrectionReason());
    assertEquals(ADDITIONAL_NOTES, response.get(0).getAdditionalNotes());
    assertEquals(CREATED_BY, response.get(0).getCreatedBy());
    assertEquals(CREATED_DATE, response.get(0).getCreatedDate());
  }

  @Test
  public void checkCategoryChangeTest() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    when(pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE)).thenReturn(new GdnBaseRestResponse(true));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, PRODUCT_CODE, false);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    assertEquals(StringUtils.EMPTY, response.getReason());
  }

  @Test
  public void checkCategoryChangeClientExceptionTest() {
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(null);
    try {
      productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, false);
    } catch (ClientException e) {
    } finally {
      verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    }
  }

  @Test
  public void checkCategoryChangeClientExceptionFirstCategoryTest() {
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(null);
    try {
      productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, false);
    } catch (ClientException e) {
    } finally {
      verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    }
  }

  @Test
  public void saveHistoryIfCategoryNotChangedTest() {
    productService.saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, CATEGORY_CODE);
  }

  @Test
  public void saveHistoryIfCategoryChangedTest() {
    productService.saveHistoryIfCategoryChanged(PRODUCT_CODE, CATEGORY_CODE, DEFAULT_CATEGORY_CODE);
    verify(pbpFeign).submitHistory(productHistoryRequestArgumentCaptor.capture());
    assertEquals(PRODUCT_CODE, productHistoryRequestArgumentCaptor.getValue().getProductCode());
    assertEquals(Constants.CATEGORY_CHANGED, productHistoryRequestArgumentCaptor.getValue().getDescription());
    assertEquals(CATEGORY_CHANGE_NOTES, productHistoryRequestArgumentCaptor.getValue().getNotes());
  }

  @AfterEach
  public void tearDown() throws IOException {
    verifyNoMoreInteractions(productWorkflowService);
    verifyNoMoreInteractions(pbpFeign);
    verifyNoMoreInteractions(bpService);
    verifyNoMoreInteractions(pcbFeign);
    verifyNoMoreInteractions(extCatalogProperties);
    verifyNoMoreInteractions(extCatalogService);
    verifyNoMoreInteractions(categoryService);
    verifyNoMoreInteractions(cacheProductService);
    verifyNoMoreInteractions(xProductFeign);
    verifyNoMoreInteractions(this.kafkaProducer);
    verifyNoMoreInteractions(systemParameterProperties);
    verifyNoMoreInteractions(clientParameterHelper);
    verifyNoMoreInteractions(bpjphFeign);
    FileUtils.deleteDirectory(new File(PATH));
  }

  @Test
  public void findDetailByProductCode() throws Exception {
    Mockito.when(
        pcbFeign.filterProductDetailByProductCodeWithOriginalImages(Mockito.anyString(), Mockito.anyString(), eq(true)))
        .thenReturn(response);
    ProductDetailResponse productDetailResponse1 =
        productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterProductDetailByProductCodeWithOriginalImages(PRODUCT_CODE, CATEGORY_CODE, true);
    assertEquals(PRODUCT_CODE, productDetailResponse1.getProductCode());
  }

  @Test
  public void findDetailByProductCodeExceptionTest() {
    Mockito.when(pcbFeign.filterProductDetailByProductCodeWithOriginalImages(PRODUCT_CODE, CATEGORY_CODE, true))
        .thenReturn(null);
    try {
      productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, CATEGORY_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign).filterProductDetailByProductCodeWithOriginalImages(PRODUCT_CODE, CATEGORY_CODE, true);
    }
  }

  @Test
  public void getAttributeInfoByAttributeCodeTest() throws Exception {
    Mockito.when(pcbFeign.getAttributeByAttributeCode(Mockito.anyString())).thenReturn(masterAttributeResponseGdnRestSingleResponse);
    MasterAttributeResponse response = productService.getAttributeInfoByAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    Mockito.verify(pcbFeign).getAttributeByAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    assertEquals(DEFAULT_ATTRIBUTE_CODE, response.getAttributeCode());
  }

  @Test
  public void getAttributeInfoByAttributeCodeExceptionTest() {
    Mockito.when(pcbFeign.getAttributeByAttributeCode(DEFAULT_ATTRIBUTE_CODE)).thenReturn(null);
    try {
      productService.getAttributeInfoByAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign).getAttributeByAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    }
  }

  @Test
  public void getAllProductsTest() throws Exception {
    gdnRestListResponse.getContent().get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    Mockito.when(pbpFeign.getAllProducts(summaryFilterRequest1, PAGE, SIZE)).thenReturn(gdnRestListResponse);
    Mockito.when(pcbFeign.getCategoryNames(any(CategoryMultipleIdRequest.class), eq(0), eq(1)))
        .thenReturn(categoryNamesResponseGdnRestSingleResponse);
    when(bpService.getProfileResponseMap(Mockito.anyList()))
        .thenReturn(profileResponseMap);
    Page<ProductSuspensionWebResponse> productSuspensionWebResponsePage =
        productService.getAllProducts(productSuspensionFilterRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(pbpFeign).getAllProducts(summaryFilterRequest1, PAGE, SIZE);
    verify(bpService).getProfileResponseMap(Mockito.anyList());
    Mockito.verify(pcbFeign)
        .getCategoryNames(categoryMultipleIdRequestArgumentCaptor.capture(), eq(0), eq(1));
    Assertions.assertEquals(1, categoryMultipleIdRequestArgumentCaptor.getValue().getCategoryCode().size());
    Assertions.assertEquals(CATEGORY_CODE, categoryMultipleIdRequestArgumentCaptor.getValue().getCategoryCode().get(0));
    List<ProductSuspensionWebResponse> productSuspensionWebResponseList = productSuspensionWebResponsePage.getContent();
    Assertions.assertEquals(1, productSuspensionWebResponseList.size());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE_1, productSuspensionWebResponseList.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME_1, productSuspensionWebResponseList.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, productSuspensionWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, productSuspensionWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(CC, productSuspensionWebResponseList.get(0).getCommissionType());
  }

  @Test
  public void getAllProductsWithCategoryCodeNullTest() throws Exception {
    suspensionProductResponse.setCategoryCode(null);
    Mockito.when(pbpFeign.getAllProducts(summaryFilterRequest1, PAGE, SIZE)).thenReturn(gdnRestListResponse);
    when(bpService.getProfileResponseMap(Mockito.anyList()))
        .thenReturn(profileResponseMap);
    Page<ProductSuspensionWebResponse> productSuspensionWebResponsePage =
        productService.getAllProducts(productSuspensionFilterRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(pbpFeign).getAllProducts(summaryFilterRequest1, PAGE, SIZE);
    verify(bpService).getProfileResponseMap(Mockito.anyList());
    Mockito.verify(pcbFeign, times(0))
        .getCategoryNames(any(CategoryMultipleIdRequest.class), eq(0), eq(1));
    List<ProductSuspensionWebResponse> productSuspensionWebResponseList = productSuspensionWebResponsePage.getContent();
    Assertions.assertEquals(1, productSuspensionWebResponseList.size());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productSuspensionWebResponseList.get(0).getBusinessPartnerCode());
    Assertions.assertNull(productSuspensionWebResponseList.get(0).getBusinessPartnerName());
    Assertions.assertNull(productSuspensionWebResponseList.get(0).getCategoryCode());
    Assertions.assertNull(productSuspensionWebResponseList.get(0).getCategoryName());
  }

  @Test
  public void getAllProductsExceptionTest() {
    Mockito.when(pbpFeign.getAllProducts(summaryFilterRequest1, PAGE, SIZE)).thenReturn(null);
    try {
      productService.getAllProducts(productSuspensionFilterRequest, PageRequest.of(PAGE, SIZE));
    } catch (Exception e) {
    } finally {
      Mockito.verify(pbpFeign).getAllProducts(summaryFilterRequest1, PAGE, SIZE);
    }
  }

  @Test
  public void doSuspensionTest() {
    Mockito.when(pbpFeign.doSuspensionAction(new SuspensionProductRequest()))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    productService.doSuspensionAction(new SuspensionProductBulkActionsWebRequest());
    Mockito.verify(pbpFeign).doSuspensionAction(new SuspensionProductRequest());
  }

  @Test
  public void doSuspensionExceptionTest() {
    Mockito.when(pbpFeign.doSuspensionAction(new SuspensionProductRequest()))
        .thenReturn(new GdnBaseRestResponse(null, null, false, null));
    try {
      productService.doSuspensionAction(new SuspensionProductBulkActionsWebRequest());
    } catch (ClientException e) {
    } finally {
      Mockito.verify(pbpFeign).doSuspensionAction(new SuspensionProductRequest());
    }
  }

  @Test
  public void getSuspensionHistoryTest() {
    List<ProductSuspensionHistoryResponse> historyList = Arrays.asList(productSuspensionHistoryResponse);
    when(pbpFeign.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE))
        .thenReturn(new GdnRestListResponse<>(historyList, new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<ProductSuspensionHistoryResponse> response =
        pbpFeign.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    verify(pbpFeign).getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    assertEquals(DEFAULT_REASON, response.getContent().get(0).getReason());
  }

  @Test
  public void getSuspensionHistoryNullTest() throws Exception {
    when(pbpFeign.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE)).thenReturn(null);
    try {
      productService.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    }
  }

  @Test
  public void getSuspensionHistoryTestReturnType() {
    List<ProductSuspensionHistoryResponse> historyList = Arrays.asList(productSuspensionHistoryResponse);
    when(pbpFeign.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE))
        .thenReturn(new GdnRestListResponse<>(historyList, new PageMetaData(), REQUEST_ID));
    Page<ProductSuspensionHistoryWebResponse> response =
        productService.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    verify(pbpFeign).getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
    assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    assertEquals(DEFAULT_REASON, response.getContent().get(0).getReason());
  }

  @Test
  public void saveProductSuspensionFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(fileStorageService.uploadFilePath(any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PATH);

    productService.saveProductSuspensionFile(multipartFile, TYPE, REQUEST_ID, STORE_ID, USERNAME);
    Mockito.verify(fileStorageService).uploadFilePath(any(), Mockito.anyString(), Mockito.anyString());
    verify(kafkaProducer).send(eq(DomainEventName.BULK_PRODUCT_SUSPENSION), eq(USERNAME),
        bulkProductSuspensionRequestArgumentCaptor.capture());
    Assertions.assertNotNull(new File(PATH + REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(bulkProductSuspensionRequestArgumentCaptor.getValue());
    Assertions.assertEquals(TYPE, bulkProductSuspensionRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(REQUEST_ID, bulkProductSuspensionRequestArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, bulkProductSuspensionRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME, bulkProductSuspensionRequestArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void updateProductCategoryTest() {
    Mockito.when(pbpFeign.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE)).thenReturn(new GdnBaseRestResponse(true));
    boolean response = productService.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE);
    Mockito.verify(pbpFeign).updateProductCategory(PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void findProductCollectionSummaryByKeywordTest() throws Exception {
    Mockito.when(pbpFeign
        .filterProductCollectionSummaryByKeyword(BUSINESS_PARTNER_CODE, CATEGORY_CODE, KEYWORD, null, true, true,
            DEFAULT_SORT_TYPE, PAGE, SIZE)).thenReturn(productCollectionResponseGdnRestListResponse);
    Page<ProductCollectionWebResponse> responsePage =
        productService.findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(pbpFeign)
        .filterProductCollectionSummaryByKeyword(BUSINESS_PARTNER_CODE, CATEGORY_CODE, KEYWORD, null, true, true,
            DEFAULT_SORT_TYPE, PAGE, SIZE);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_CODE, responsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(CATEGORY_CODE, responsePage.getContent().get(0).getCategoryCode());
  }

  @Test
  public void findProductCollectionSummaryByKeywordWithStartAgeAndEndAgeTest() throws Exception {
    summaryFilterWebRequest.setTimeFilterType(TimeFilterType.TODAY.getTimeFilterType());
    Mockito.when(pbpFeign
        .filterProductCollectionSummaryByKeywordAndAgeBetween(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE, KEYWORD,
            null, null, TimeFilterType.TODAY.getTimeFilterType(), true, true))
        .thenReturn(productCollectionResponseGdnRestListResponse);
    Page<ProductCollectionWebResponse> responsePage =
        productService.findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(pbpFeign)
        .filterProductCollectionSummaryByKeywordAndAgeBetween(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE, KEYWORD,
            null, null, TimeFilterType.TODAY.getTimeFilterType(), true, true);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_CODE, responsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(CATEGORY_CODE, responsePage.getContent().get(0).getCategoryCode());
  }

  @Test
  public void findProductCollectionSummaryByKeywordWithStartAgeTest() throws Exception {
    summaryFilterWebRequest.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO.getTimeFilterType());
    Mockito.when(pbpFeign
        .filterProductCollectionSummaryByKeywordAndAgeLessThan(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE,
            KEYWORD, null, TimeFilterType.FIVE_DAYS_AGO.getTimeFilterType(), true, true))
        .thenReturn(productCollectionResponseGdnRestListResponse);
    Page<ProductCollectionWebResponse> responsePage =
        productService.findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE));
    Mockito.verify(pbpFeign)
        .filterProductCollectionSummaryByKeywordAndAgeLessThan(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE,
            KEYWORD, null, TimeFilterType.FIVE_DAYS_AGO.getTimeFilterType(), true, true);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_CODE, responsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(CATEGORY_CODE, responsePage.getContent().get(0).getCategoryCode());
  }

  @Test
  public void findProductCollectionSummaryByKeywordInvalidFilterTypeTest() throws Exception {
    summaryFilterWebRequest.setTimeFilterType(TimeFilterType.ALL.name());
    try{
    Page<ProductCollectionWebResponse> responsePage =
        productService.findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE));
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }


  public void getFilterCountsBySourceTest_forFinalQC() throws Exception {
    countWebRequest.setSource(Source.FINAL_QC.name());
    countWebRequest.setStatus(WorkflowState.PASSED.name());
    Mockito.when(pdtFeign.countDistributionSummaryByFilter(eq(false), eq(false),
        any(DistributionTaskMultipleFilterRequest.class)))
        .thenReturn(new GdnRestSingleResponse<MapResponse>(new MapResponse(), REQUEST_ID));
    MapWebResponse response = productService.getFilterCountsBySource(countWebRequest);
    Mockito.verify(pdtFeign).countDistributionSummaryByFilter(eq(false), eq(false),
        distributionTaskMultipleFilterRequestArgumentCaptor.capture());
    Assertions.assertEquals(2, distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getStatusList().size());
    Assertions.assertNotNull(response);
  }

  @Test
  public void getFilterCountsBySourceTest_forDistributionList() throws Exception {
    countWebRequest.setSource(Source.DISTRIBUTION_LIST.name());
    Mockito.when(pdtFeign.countDistributionSummaryByFilter(eq(true), eq(false),
        any(DistributionTaskMultipleFilterRequest.class)))
        .thenReturn(new GdnRestSingleResponse<MapResponse>(new MapResponse(), REQUEST_ID));
    MapWebResponse response = productService.getFilterCountsBySource(countWebRequest);
    Mockito.verify(pdtFeign).countDistributionSummaryByFilter(eq(true), eq(false),
        distributionTaskMultipleFilterRequestArgumentCaptor.capture());
    Assertions.assertNull(distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getStatusList());
    Assertions.assertNotNull(response);
  }

  @Test
  public void getFilterCountsBySourceTest_forInProgress() throws Exception {
    countWebRequest.setSource(Source.IN_PROGRESS.name());
    Mockito
        .when(pbpFeign.countProductCollectionBySpecifiedDateRange(MERCHANT_CODE, CATEGORY_CODE, KEYWORD, true, false))
        .thenReturn(new GdnRestSingleResponse<>(new ProductCollectionCountRestResponse(), REQUEST_ID));
    MapWebResponse response = productService.getFilterCountsBySource(countWebRequest);
    Mockito.verify(pbpFeign)
        .countProductCollectionBySpecifiedDateRange(MERCHANT_CODE, CATEGORY_CODE, KEYWORD, true, false);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getFilterCountsBySourceExceptionTest() throws Exception {
    countWebRequest.setSource(Source.ACTIVE.name());
    try {
      productService.getFilterCountsBySource(countWebRequest);
    } catch (ApplicationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void bulkUpdateMasterProductDataTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getBulkUploadFilePath()).thenReturn(PATH);
    Mockito.when(fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_BULK_UPLOAD)).thenReturn(PATH);
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.BULK_INTERNAL_PROCESS_CODE_KEY, Long.valueOf(1)), Constants.REQUEST_ID));
    productService.bulkUpdateMasterProductData(multipartFile, REQUEST_ID, STORE_ID, USERNAME);
    Mockito.verify(pbpFeign).findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    verify(kafkaProducer).send(eq(DomainEventName.INTERNAL_USER_BULK_UPLOAD_EVENT), eq(USERNAME),
        masterDataBulkUpdateRequestArgumentCaptor.capture());
    verify(fileStorageService).uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_BULK_UPLOAD);
    Assertions.assertNotNull(new File(PATH + REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(masterDataBulkUpdateRequestArgumentCaptor.getValue());
    Assertions.assertEquals(REQUEST_ID, masterDataBulkUpdateRequestArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, masterDataBulkUpdateRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME, masterDataBulkUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void downloadBulkSelectedMasterProductsForInternalTest() {
    this.productService
        .downloadBulkSelectedMasterProductsForInternal(Constants.USER_NAME, selectedMasterProductDownloadWebRequest);
    Mockito.verify(this.kafkaProducer).send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
        selectedMasterProductDownloadRequestArgumentCaptor.capture());
    SelectedMasterProductDownloadRequest bulkDownloadRequest =
        selectedMasterProductDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(bulkDownloadRequest);
    Assertions.assertEquals(PRODUCT_CODE, bulkDownloadRequest.getProductCodes().get(0));
  }

  @Test
  public void downloadBulKMasterProductsTest() {
    this.productService.downloadBulKMasterProducts(Constants.USER_NAME, summaryFilterWebRequest);
    Mockito.verify(this.kafkaProducer).send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
        masterProductDownloadRequestArgumentCaptor.capture());
    MasterProductDownloadRequest bulkDownloadRequest = masterProductDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(bulkDownloadRequest);
    Assertions.assertEquals(KEYWORD, bulkDownloadRequest.getFilterName());
    Assertions.assertEquals(CATEGORY_CODE, bulkDownloadRequest.getCategoryCode());
    Assertions.assertNull(bulkDownloadRequest.getReviewPending());
  }

  @Test
  public void findProductHistoryForActiveProductTest() {
    Mockito.when(pbpFeign.getProductHistorySummary(PAGE, SIZE, PRODUCT_ID))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(productHistoryResponse), new PageMetaData(), REQUEST_ID));
    Page<HistoryWebResponse> responsePage =
        productService.findProductHistory(ACTIVE, PRODUCT_CODE, PRODUCT_ID, PAGE, SIZE);
    Mockito.verify(pbpFeign).getProductHistorySummary(PAGE, SIZE, PRODUCT_ID);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_ID, responsePage.getContent().get(0).getProductId());
    Assertions.assertEquals(HISTORY_DESCRIPTION, responsePage.getContent().get(0).getActivity());
    Assertions.assertEquals(NOTES, responsePage.getContent().get(0).getDescription());
  }

  @Test
  public void findProductHistoryForFinalQCTest() {
    Mockito.when(pdtFeign.getProductHistories(PAGE, SIZE, PRODUCT_CODE, true))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(taskHistoryResponse), new PageMetaData(), REQUEST_ID));
    Page<HistoryWebResponse> responsePage =
        productService.findProductHistory(FINAL_QC, PRODUCT_CODE, PRODUCT_ID, PAGE, SIZE);
    Mockito.verify(pdtFeign).getProductHistories(PAGE, SIZE, PRODUCT_CODE, true);
    Assertions.assertNotNull(responsePage.getContent());
    Assertions.assertEquals(PRODUCT_CODE, responsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(HISTORY_DESCRIPTION, responsePage.getContent().get(0).getReason());
    Assertions.assertEquals(CATEGORY_CODE, responsePage.getContent().get(0).getCategoryCode());
  }

  @Test
  public void findProductHistoryExceptionTest() {
    try {
      productService.findProductHistory(ACTION, PRODUCT_CODE, PRODUCT_ID, PAGE, SIZE);
    } catch (ApplicationRuntimeException ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void retryProductPublishToPDT() {
    when(pdtFeign.isProductExists(PRODUCT_CODE))
        .thenReturn(new com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse<>(REQUEST_ID, false));
    when(pbpFeign.publishProductToPDT(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(REQUEST_ID));
    boolean value = this.productService.retryProductPublishToPDT(PRODUCT_CODE);
    Mockito.verify(this.pdtFeign).isProductExists(PRODUCT_CODE);
    Mockito.verify(this.pbpFeign).publishProductToPDT(PRODUCT_CODE);
    assertTrue(value);
  }

  @Test
  public void retryProductPublishToPDTExceptionTest() {
    when(pdtFeign.isProductExists(PRODUCT_CODE))
        .thenReturn(new com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse<>(REQUEST_ID, true));
    boolean value = this.productService.retryProductPublishToPDT(PRODUCT_CODE);
    Mockito.verify(this.pdtFeign).isProductExists(PRODUCT_CODE);
    Mockito.verify(this.pbpFeign, times(0)).publishProductToPDT(PRODUCT_CODE);
    assertFalse(value);
  }


  //TODO check on the usage of reflection used in GdnObjects.equals)
  @Test
  public void checkCategoryChangeTest_differentWholesaleConfig() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse2, REQUEST_ID));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, true);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    assertEquals(WHOLESALE_CONFIGURATIONS_MISMATCH_MESSAGE, response.getReason());
  }

  @Test
  public void checkCategoryChangeTest_differentWholesaleConfigType() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    categoryDetailResponse.getCategoryCode();
    when(pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE)).thenReturn(new GdnBaseRestResponse(false));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, false);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    assertEquals(WHOLESALE_CONFIGURATIONS_MISMATCH_MESSAGE, response.getReason());
    assertFalse(response.isCategoryChangeSupported());
  }

  @Test
  public void checkCategoryChangeTest_differentWholesaleConfigMinDiscount() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    wholesaleMappingResponse2.getWholesaleConfig().get(0).getMinWholesaleDiscount().get(0).setPrice(0.0);
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse2, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, true);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    assertEquals(WHOLESALE_CONFIGURATIONS_MISMATCH_MESSAGE, response.getReason());
  }

  @Test
  public void checkCategoryChangeTest_targetCategoryWholesaleDisabled() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(false);
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, false);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    assertEquals(StringUtils.EMPTY, response.getReason());
  }

  @Test
  public void checkCategoryChangeTest_differentDefiningAttributes() {
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    CategoryDetailResponse categoryDetailResponse2 = generateCategoryDetailResponse();
    categoryDetailResponse2.setCategoryAttributes(new ArrayList<>());
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse2, REQUEST_ID));
    CategoryChangeCheckResponse response =
        productService.checkCategoryChange(DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, PRODUCT_CODE, true);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    assertEquals(DEFINING_ATTRIBUTE_MISMATCH_MESSAGE, response.getReason());
  }

  @Test
  public void reindexByProductSkus() {
    Mockito.when(xProductFeign
        .reindexByProductSkus(eq(STORE_ID), eq(Constants.REQUEST_ID), any(SimpleListStringRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    productService.reindexByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductFeign)
        .reindexByProductSkus(eq(STORE_ID), eq(Constants.REQUEST_ID), any(SimpleListStringRequest.class));
  }

  @Test
  public void reindexByProductCodeTest() {
    Mockito.when(pbpFeign.reindexByProductCode(eq(PRODUCT_CODE))).thenReturn(new GdnBaseRestResponse(true));
    productService.reindexByProductCode(PRODUCT_CODE);
    Mockito.verify(pbpFeign).reindexByProductCode(eq(PRODUCT_CODE));
  }

  @Test
  public void reindexByProductSkusException() {
    Mockito.when(xProductFeign
        .reindexByProductSkus(eq(STORE_ID), eq(Constants.REQUEST_ID), any(SimpleListStringRequest.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      productService.reindexByProductSku(STORE_ID, PRODUCT_SKU);
    } catch (Exception e) {
      Assertions.assertEquals(ClientException.class, e.getClass());
    } finally {
      Mockito.verify(xProductFeign)
          .reindexByProductSkus(eq(STORE_ID), eq(Constants.REQUEST_ID), any(SimpleListStringRequest.class));
    }
  }

  @Test
  public void retryProductNeedRevisionToPBPTest() {
    productService.retryProductNeedRevisionToPBP(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer).send(
        eq(com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME),
        eq(PRODUCT_CODE), pdtNeedRevisionEventModelArgumentCaptor.capture());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTest() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    categoryDetailResponse.setBopisEligible(false);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE),
            Mockito.anyString()))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID,
        DEFAULT_CATEGORY_ID, true, PRODUCT_TYPE);
    verify(pcbFeign, times(2)).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2)).filterMarginCategoryByCategoryCodeAndOrderDate(
        eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponse_differentMarginTest() throws Exception {
    MarginCategoryResponse marginCategoryResponse2 = new MarginCategoryResponse();
    marginCategoryResponse2.setValue(2000.0);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE),
            Mockito.anyString()))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse2, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID,
        DEFAULT_CATEGORY_ID,true, PRODUCT_TYPE);
    verify(pcbFeign, times(2)).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2)).filterMarginCategoryByCategoryCodeAndOrderDate(
        eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTest_isActiveFalse() throws Exception {
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(false);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTest_isActiveTrue() throws Exception {
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(new GdnBaseRestResponse(false));
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTest_isActiveTrue_SuccessTrue() throws Exception {
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTest_Exception() throws Exception {
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestRegularProduct() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, "1");
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestNullProductType() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, null);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestRegularProductCheckFalse() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, "1");
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProduct() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", false);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductCheck() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductCheckOld() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", false);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse.setBopisEligible(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    CategoryDetailResponse categoryDetailResponse2 = gdnRestSingleResponse2.getValue();
    categoryDetailResponse2.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse2.setBopisEligible(false);
    gdnRestSingleResponse2.setValue(categoryDetailResponse2);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductBothEligibleNotBopisType() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(true);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "Non-BOPIS_TYPE");

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductNotEligibleToEligibleNotBopisType() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "Non-BOPIS_TYPE");

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductNotEligibleToEligibleWithBopisType() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductNotEligibleToEligibleWithBopis() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductBothNotEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }



  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductNotEligibleToEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductBothNotEligibleWithBopisType() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }



  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductCheckNew() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse.setBopisEligible(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    CategoryDetailResponse categoryDetailResponse2 = gdnRestSingleResponse2.getValue();
    categoryDetailResponse2.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse2.setBopisEligible(false);
    gdnRestSingleResponse2.setValue(categoryDetailResponse2);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "1");
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductBothEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(true);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductBothEligibleNot() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(true);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "1");

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }


  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductCheckEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(true);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);
    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", false);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "1");
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(true);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);
    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", false);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductNotEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);

    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);

    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);

    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));

    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);

    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);

    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);

    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));

    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);

    // Verify interactions
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }


  @Test
  public void getProductCategoryChangeResponseTestBopisProduct() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);
    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(true);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestProductEligible() throws Exception {
    CategoryDetailResponse categoryDetailResponse3 = new CategoryDetailResponse();
    categoryDetailResponse3.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse3.setBopisEligible(false);
    categoryDetailResponse3.setCategoryCode(DEFAULT_CATEGORY_CODE);
    CategoryDetailResponse categoryDetailResponse4 = new CategoryDetailResponse();
    categoryDetailResponse4.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse4.setBopisEligible(false);
    categoryDetailResponse4.setCategoryCode(DEFAULT_CATEGORY_ID_2);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse3, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse4, REQUEST_ID);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_ID_2), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, "1");
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(any(), any());
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getProductCategoryChangeCheckResponseTestBopisProductCheckBopisEligible() throws Exception {
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    GdnRestSingleResponse<CategoryDetailResponse> gdnRestSingleResponse2 =
        new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID);
    CategoryDetailResponse categoryDetailResponse = gdnRestSingleResponse.getValue();
    categoryDetailResponse.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse.setBopisEligible(true);
    gdnRestSingleResponse.setValue(categoryDetailResponse);
    CategoryDetailResponse categoryDetailResponse2 = gdnRestSingleResponse2.getValue();
    categoryDetailResponse2.setWholesalePriceConfigEnabled(true);
    categoryDetailResponse2.setBopisEligible(false);
    gdnRestSingleResponse.setValue(categoryDetailResponse2);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID)).thenReturn(gdnRestSingleResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2)).thenReturn(gdnRestSingleResponse2);
    Mockito.when(this.pbpFeign.compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(null);
    Mockito.when(
            marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(eq(DEFAULT_CATEGORY_CODE), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, marginCategoryResponse, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2, false, PRODUCT_TYPE);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pbpFeign).compareProductAndCategoryWholesale(PRODUCT_CODE, DEFAULT_CATEGORY_CODE);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign, times(1)).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    verify(marginFeign, times(2))
        .filterMarginCategoryByCategoryCodeAndOrderDate(any(), Mockito.anyString());
  }

  @Test
  public void getHalalProductHistoryTest() {
    when(pbpFeign.getHalalProductHistory(PRODUCT_SKU, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(halalProductHistoryResponse), new PageMetaData(), REQUEST_ID));
    Page<HalalProductHistoryWebResponse> response = productService.getHalaProductHistory(PRODUCT_SKU, PAGE, SIZE);
    verify(pbpFeign).getHalalProductHistory(PRODUCT_SKU, PAGE, SIZE);
    assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    assertEquals(CURRENT_VALUE, response.getContent().get(0).getCurrentValue());
    assertEquals(PREVIOUS_VALUE, response.getContent().get(0).getPreviousValue());
    assertEquals(ACTIVITY, response.getContent().get(0).getActivity());
  }

  @Test
  public void getHalalProductHistoryExceptionTest() {
    when(pbpFeign.getHalalProductHistory(PRODUCT_SKU, PAGE, SIZE)).thenReturn(null);
    try {
      productService.getHalaProductHistory(PRODUCT_SKU, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pbpFeign).getHalalProductHistory(PRODUCT_SKU, PAGE, SIZE);
    }
  }

  @Test
  public void getInternalDownloadTemplateFilePathsTest() {
    when(fileStorageService.getInternalDownloadTemplateFilePaths()).thenReturn(new HashMap<>());
    productService.getInternalDownloadTemplateFilePaths();
    verify(fileStorageService).getInternalDownloadTemplateFilePaths();
  }

  @Test
  public void getHalalProductDetailsByProductSkuTest() {
    ReflectionTestUtils.setField(productService, "attributeCode", ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(productService, "halalProductLinkPrefix", HALAL_PRODUCT_PREFIX);
    GdnRestListResponse<HalalProductResponse> halalProductResponseList = new GdnRestListResponse<>();
    List<HalalProductResponse> halalProductResponsesList = new ArrayList<>();
    HalalProductResponse halalProductResponse = new HalalProductResponse();
    halalProductResponse.setHalalProduct(true);
    halalProductResponse.setProductName(PRODUCT_NAME);
    halalProductResponse.setProductSku(PRODUCT_SKU);
    halalProductResponse.setProductCode(PRODUCT_CODE);
    halalProductResponse.setCurationStatus(ACTION);
    halalProductResponsesList.add(halalProductResponse);
    halalProductResponseList.setContent(halalProductResponsesList);
    halalProductResponseList.setSuccess(true);
    GdnRestSingleResponse<SingleObjectResponse> certificateNumberResponse = new GdnRestSingleResponse<>();
    SingleObjectResponse singleObjectResponse = new SingleObjectResponse(CERTIFICATE_NUMBER);
    certificateNumberResponse.setSuccess(true);
    certificateNumberResponse.setValue(singleObjectResponse);
    Mockito.when(xProductFeign.getProductDetailsByProductSkuList(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(halalProductResponseList);
    Mockito.when(pcbFeign.getAttributeValuesByProductCodeAndAttributeCode(PRODUCT_CODE, ATTRIBUTE_CODE))
        .thenReturn(certificateNumberResponse);
    HalalProductWebResponse halalProductResponse1 = productService.getHalalProductDetailsByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign).getProductDetailsByProductSkuList(any());
    Mockito.verify(pcbFeign).getAttributeValuesByProductCodeAndAttributeCode(anyString(), any());
    Assertions.assertEquals(PRODUCT_CODE, halalProductResponse1.getProductCode());
    Assertions.assertEquals(CERTIFICATE_NUMBER, halalProductResponse1.getCertificateNumber());
    Assertions.assertEquals(ACTION, halalProductResponse1.getCurationStatus());
  }

  @Test
  public void getHalalProductDetailsByProductSkuNullTest() {
    ReflectionTestUtils.setField(productService, "attributeCode", ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(productService, "halalProductLinkPrefix", HALAL_PRODUCT_PREFIX);
    GdnRestListResponse<HalalProductResponse> halalProductResponseList = new GdnRestListResponse<>();
    List<HalalProductResponse> halalProductResponsesList = new ArrayList<>();
    HalalProductResponse halalProductResponse = new HalalProductResponse();
    halalProductResponse.setHalalProduct(true);
    halalProductResponse.setProductName(PRODUCT_NAME);
    halalProductResponse.setProductSku(PRODUCT_SKU);
    halalProductResponse.setCurationStatus(ACTION);
    halalProductResponsesList.add(halalProductResponse);
    halalProductResponseList.setContent(halalProductResponsesList);
    halalProductResponseList.setSuccess(true);
    halalProductResponseList.setContent(null);
    Mockito.when(xProductFeign.getProductDetailsByProductSkuList(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(halalProductResponseList);
    HalalProductWebResponse halalProductResponse1 = productService.getHalalProductDetailsByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign).getProductDetailsByProductSkuList(any());
    Assertions.assertEquals(null, halalProductResponse1.getCertificateNumber());
    Assertions.assertEquals(null, halalProductResponse1.getCurationStatus());
  }

  @Test
  public void getHalalProductDetailsByProductSkuPcbExceptionTest() {
    ReflectionTestUtils.setField(productService, "attributeCode", ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(productService, "halalProductLinkPrefix", HALAL_PRODUCT_PREFIX);
    GdnRestListResponse<HalalProductResponse> halalProductResponseList = new GdnRestListResponse<>();
    List<HalalProductResponse> halalProductResponsesList = new ArrayList<>();
    HalalProductResponse halalProductResponse = new HalalProductResponse();
    halalProductResponse.setHalalProduct(true);
    halalProductResponse.setProductName(PRODUCT_NAME);
    halalProductResponse.setProductSku(PRODUCT_SKU);
    halalProductResponse.setProductCode(PRODUCT_CODE);
    halalProductResponse.setCurationStatus(ACTION);
    halalProductResponsesList.add(halalProductResponse);
    halalProductResponseList.setContent(halalProductResponsesList);
    halalProductResponseList.setSuccess(true);
    GdnRestSingleResponse<SingleObjectResponse> certificateNumberResponse = new GdnRestSingleResponse<>();
    SingleObjectResponse singleObjectResponse = new SingleObjectResponse(CERTIFICATE_NUMBER);
    certificateNumberResponse.setSuccess(false);
    certificateNumberResponse.setValue(singleObjectResponse);
    Mockito.when(xProductFeign.getProductDetailsByProductSkuList(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(halalProductResponseList);
    Mockito.when(pcbFeign.getAttributeValuesByProductCodeAndAttributeCode(PRODUCT_CODE, ATTRIBUTE_CODE))
        .thenReturn(certificateNumberResponse);
    HalalProductWebResponse halalProductResponse1 = productService.getHalalProductDetailsByProductSku(PRODUCT_SKU);
    Mockito.verify(xProductFeign).getProductDetailsByProductSkuList(any());
    Mockito.verify(pcbFeign).getAttributeValuesByProductCodeAndAttributeCode(anyString(), any());
    Assertions.assertEquals(PRODUCT_CODE, halalProductResponse1.getProductCode());
    Assertions.assertEquals(ACTION, halalProductResponse1.getCurationStatus());
    Assertions.assertEquals(StringUtils.EMPTY, halalProductResponse1.getCertificateNumber());
  }

  @Test
  public void getHalalCertificationDetailsTest() {
    ReflectionTestUtils.setField(productService, "halalThirdPartyApiSwitch", true);
    ReflectionTestUtils.setField(productService, "halalApiKey", HALAL_API_KEY);
    BPJPHListResponse<HalalCertificationDetailResponse> response1 = new BPJPHListResponse<>();
    HalalCeritficationDetails halalCeritficationDetails = new HalalCeritficationDetails();
    halalCeritficationDetails.setNo_sert(CERTIFICATE_NUMBER);
    HalalCertificationDetailResponse halalCertificationDetailResponse = new HalalCertificationDetailResponse();
    List<HalalCertificationDetailResponse> halalCertificationDetailResponses = new ArrayList<>();
    halalCertificationDetailResponse.setReg_prod_name(PRODUCT_NAME);
    halalCertificationDetailResponse.setSertifikat(halalCeritficationDetails);
    halalCertificationDetailResponses.add(halalCertificationDetailResponse);
    BPJPHData bpjphData = new BPJPHData();
    bpjphData.setDatas(halalCertificationDetailResponses);
    bpjphData.setCurrent_page(0);
    bpjphData.setTotal_items(20L);
    bpjphData.setTotal_pages(30);
    response1.setStatusCode(200);
    response1.setData(bpjphData);
    response1.setError(false);
    response1.setMessage(null);
    Mockito.when(bpjphFeign.getHalalCertificationDetails(HALAL_API_KEY, PAGE, SIZE, CERTIFICATE_NUMBER)).thenReturn(response1);
    Page<HalalCertificationWebDetailsResponse> responsePage =
        productService.getHalalCertificationDetails(CERTIFICATION_NUMBER, PAGE, SIZE);
    Mockito.verify(bpjphFeign).getHalalCertificationDetails(HALAL_API_KEY, PAGE, SIZE, CERTIFICATE_NUMBER);
    Assertions.assertEquals(CERTIFICATE_NUMBER, responsePage.getContent().get(0).getCertificationNumber());
    Assertions.assertEquals(PRODUCT_NAME, responsePage.getContent().get(0).getProductName());
  }

  @Test
  public void getHalalCertificationDetailsFalseTest() {
    ReflectionTestUtils.setField(productService, "halalThirdPartyApiSwitch", false);
    BPJPHListResponse<HalalCertificationDetailResponse> response1 = new BPJPHListResponse<>();
    HalalCeritficationDetails halalCeritficationDetails = new HalalCeritficationDetails();
    halalCeritficationDetails.setNo_sert(CERTIFICATE_NUMBER);
    HalalCertificationDetailResponse halalCertificationDetailResponse = new HalalCertificationDetailResponse();
    List<HalalCertificationDetailResponse> halalCertificationDetailResponses = new ArrayList<>();
    halalCertificationDetailResponse.setReg_prod_name(PRODUCT_NAME);
    halalCertificationDetailResponse.setSertifikat(halalCeritficationDetails);
    halalCertificationDetailResponses.add(halalCertificationDetailResponse);
    Page<HalalCertificationWebDetailsResponse> responsePage =
        productService.getHalalCertificationDetails(CERTIFICATION_NUMBER, PAGE, SIZE);
  }
  @Test
  public void getHalalDashboardProductsResponseTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    HalalProductsFilterWebRequest halalProductsFilterWebRequest = new HalalProductsFilterWebRequest();
    HalalDashboardProductsResponse halalDashboardProductsResponse = new HalalDashboardProductsResponse();
    when(xProductFeign.getHalalDashboardProducts(PAGE, SIZE, halalProductsFilterRequest)).thenReturn(
        new GdnRestListResponse<>(List.of(halalDashboardProductsResponse), new PageMetaData(), REQUEST_ID));
    Page<HalalDashboardProductsWebResponse> response =
        productService.getHalalDashboardProductsResponses(PAGE, SIZE, halalProductsFilterWebRequest);
    verify(xProductFeign).getHalalDashboardProducts(PAGE, SIZE, halalProductsFilterRequest);
    Assertions.assertEquals(1, response.getTotalElements());
  }

  @Test
  public void getHalalDashboardProductsEmptyResponseTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    HalalProductsFilterWebRequest halalProductsFilterWebRequest = new HalalProductsFilterWebRequest();
    when(xProductFeign.getHalalDashboardProducts(PAGE, SIZE, halalProductsFilterRequest)).thenReturn(null);
    try {
      productService.getHalalDashboardProductsResponses(PAGE, SIZE, halalProductsFilterWebRequest);
    } catch (ClientException e) {
    } finally {
      verify(xProductFeign).getHalalDashboardProducts(PAGE, SIZE, halalProductsFilterRequest);
    }
  }

  @Test
  public void updateHalalConfigOfProductTest() {
    Mockito.when(xProductFeign.updateHalalConfigOfProduct(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU),
        eq(CURATION_STATUS))).thenReturn(new GdnBaseRestResponse(true));
    productService.updateHalalConfigOfProduct(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, CURATION_STATUS);
    Mockito.verify(xProductFeign)
        .updateHalalConfigOfProduct(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(CURATION_STATUS));
  }

  @Test
  public void getProductCategoryChangeCheckResponseBothHalalCategoriesTest() throws Exception {
    categoryDetailResponse.setB2bExclusive(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    CategoryChangeWebResponse categoryChangeWebResponse = productService
        .getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID,
        DEFAULT_CATEGORY_ID, true, PRODUCT_TYPE);
    verify(pcbFeign, times(2)).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign, times(2)).getCategoryDetail(DEFAULT_CATEGORY_ID);
    Assertions.assertFalse(categoryChangeWebResponse.getCategoryMarginMismatch());
  }

  @Test
  public void getProductCategoryChangeCheckResponseOldHalalCategoriesTest() throws Exception {
    categoryDetailResponse.setB2bExclusive(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true,
            categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true,
            categoryDetailResponse1, REQUEST_ID));
    CategoryChangeWebResponse categoryChangeWebResponse = productService
        .getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID,
            DEFAULT_CATEGORY_ID_2, true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    Assertions.assertTrue(categoryChangeWebResponse.getCategoryMarginMismatch());
  }

  @Test
  public void getProductCategoryChangeCheckResponseNewHalalCategoriesTest() throws Exception {
    categoryDetailResponse1.setB2bExclusive(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true,
            categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true,
            categoryDetailResponse1, REQUEST_ID));
    CategoryChangeWebResponse categoryChangeWebResponse = productService
        .getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID,
            DEFAULT_CATEGORY_ID_2, true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    Assertions.assertTrue(categoryChangeWebResponse.getCategoryMarginMismatch());
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginTest() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ReflectionTestUtils.setField(productService, "setDefaultOrderTypeForMargin", true);
    ReflectionTestUtils.setField(productService, "defaultOrderTypeForMargin", B2C_RETAIL);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    categoryDetailResponse1.setBopisEligible(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(clientParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(marginFeign, times(2)).filterMargin(Mockito.anyString(), eq(null), eq(null), eq(null), eq(null),
        filterMarginsByOrderItemsRequestArgumentCaptor.capture());
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    Assertions.assertEquals(B2C_RETAIL,
        filterMarginsByOrderItemsRequestArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType());
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginSwitchFalseTest() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ReflectionTestUtils.setField(productService, "setDefaultOrderTypeForMargin", false);
    ReflectionTestUtils.setField(productService, "checkProductTypeSwitch", true);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(marginFeign, times(2)).filterMargin(Mockito.anyString(), eq(null), eq(null), eq(null), eq(null),
        filterMarginsByOrderItemsRequestArgumentCaptor.capture());
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
    Assertions.assertNull(
        filterMarginsByOrderItemsRequestArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType());
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginTest2() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("-");
    margin.setReplacementType("-");
    marginList.add(null);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginTest3() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE-ADDON");
    margin.setMarginPercentage(10.0);
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginTest5() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("-");
    margin.setMarginPercentage(10.0);
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
  }

  @Test
  public void getProductCategoryChangeCheckNewMarginTest6() throws Exception {
    ReflectionTestUtils.setField(productService, "marginNewChangesEnabled", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE");
    margin.setMarginPercentage(10.0);
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when(pcbFeign.getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2)).thenReturn(
        new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    when((pcbFeign.filterProductDetailByProductCode(PRODUCT_CODE))).thenReturn(
        new GdnRestSingleResponse<>(productDetailResponse, REQUEST_ID));
    Mockito.when(marginFeign.filterMargin(any(), any(), any(), any(), any(),
        any())).thenReturn(listBaseResponse);
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse, REQUEST_ID));
    Mockito.when(this.pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID_2))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, categoryDetailResponse1, REQUEST_ID));
    productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, DEFAULT_CATEGORY_ID, DEFAULT_CATEGORY_ID_2,
        true, PRODUCT_TYPE);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getWholesaleConfigToCategory(DEFAULT_CATEGORY_ID_2);
    verify(pcbFeign).filterProductDetailByProductCode(PRODUCT_CODE);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID_2);
  }

  @Test
  public void getProductBasicDetailsTest() {
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    GdnRestListResponse<ProductBasicResponse> listResponse =
        new GdnRestListResponse<>(List.of(productBasicResponse), null, REQUEST_ID);
    Mockito.when(xProductFeign.getProductBasicDetails(any())).thenReturn(listResponse);
    List<ProductBasicResponse> productBasicResponseList = productService.getProductBasicDetails(List.of(PRODUCT_SKU));
    Mockito.verify(xProductFeign).getProductBasicDetails(any(SimpleListStringRequest.class));
    Assertions.assertEquals(listResponse.getContent(), productBasicResponseList);
  }

  @Test
  void getProductL3BasicDetailsTest() {
    GdnRestSingleResponse<ProductL3BasicResponse> l3BasicResponse =
        new GdnRestSingleResponse<>(ProductL3BasicResponse.builder().build(), REQUEST_ID);
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(l3BasicResponse);
    ProductL3BasicResponse productL3BasicResponse =
        productService.getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    Assertions.assertNotNull(productL3BasicResponse);
  }

  @Test
  void getProductL3BasicDetailsTest_nullResponse() {
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(null);
    ProductL3BasicResponse productL3BasicResponse =
        productService.getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    Assertions.assertNull(productL3BasicResponse);
  }

  @Test
  void getProductL3BasicDetailsTest_successFalse() {
    GdnRestSingleResponse<ProductL3BasicResponse> l3BasicResponse =
        new GdnRestSingleResponse<>();
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(l3BasicResponse);
    ProductL3BasicResponse productL3BasicResponse =
        productService.getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    Assertions.assertNull(productL3BasicResponse);
  }

  @Test
  void getProductL3BasicDetailsTest_nullValue() {
    GdnRestSingleResponse<ProductL3BasicResponse> l3BasicResponse =
        new GdnRestSingleResponse<>();
    l3BasicResponse.setSuccess(true);
    Mockito.when(pbpFeign.getProductLevel3BasicDetails(PRODUCT_CODE)).thenReturn(l3BasicResponse);
    ProductL3BasicResponse productL3BasicResponse =
        productService.getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(pbpFeign).getProductLevel3BasicDetails(PRODUCT_CODE);
    Assertions.assertNull(productL3BasicResponse);
  }

}
