package com.gdn.partners.pcu.internal.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ImageQcConstants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.ProductMTAWrapper;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.VendorSummaryDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.BulkDeleteProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageFeedbackWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PrimaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectReason;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoAssignmentFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.AttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkUpdatePendingWebResposne;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSkuUpdateHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductAttributeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductItemResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableMap;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class VendorServiceImplTest {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int TOTAL_RECORDS = 1;
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
  private static final String DEFAULT_REQUEST_ID = "REQUEST-ID";
  private static final String PRODUCT_CODE = "product-code";
  private static final String REQUEST_ID = "requestId";
  private static final String USER_NAME = "username";
  private static final String NOTES_ASSERTION = "{field: 'Unique Selling Point', oldValue: 'usp', newValue: 'uspusp'}";
  private static final String NOTES = "Diubah : [{field: 'Unique Selling Point', oldValue: usp, newValue: uspusp}]";
  private static final String SKU_NOTES =
      "Diubah : [{\"skuName\":null,\"field\":\"field1\",\"oldValue\":\"oldVal1\",\"newValue\":\"newVal1\"},"
          + "{\"skuName\":\"skuName2\",\"field\":\"field2\",\"oldValue\":\"oldVal2\",\"newValue\":\"newVal2\"},"
          + "{\"skuName\":null,\"field\":\"field3\",\"oldValue\":\"oldVal3\",\"newValue\":\"newVal3\"}]";
  private static final String HISTORY_DESCRIPTION = "Diubah";
  private static final String DESCRIPTION = "Diubah";
  private static final String NAME = "name";
  private static final String URL = "url";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String REJECT_REASON = "rejectReason";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String ACTION = "Action";
  private static final String TYPE = "type";
  private static final String ASSIGNEE_USERNAME = "assignee_username";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-0001";
  private static final String DEFAULT_ASSIGNEE_NAME= "blibli";
  private static final String ITEM_ID = "ID";
  private static final String ITEM_NAME = "NAME";
  private static final String LOCATION_PATH = "locationPath";
  private static final String LOCATION_PATH_1 = "locationPath1";
  private static final String LOCATION_PATH_2 = "locationPath2";
  private static final String LOCATION_PATH_3 = "locationPath3";
  private static final boolean MAIN_IMAGE = true;
  private static final String STATE = "REJECTED";
  private static final String DEFAULT_TIMEFILTER_TYPE_FOR_ALL = "ALL";
  private static final String SKU_CODE = "skuCode";
  private static final String CATEGORY = "Categories";
  private static final String ATTRIBUTE_ID = "ID";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String VALUE = "VALUE";
  private static final String SKU_NAME = "skuName2";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String STORE_ID = "storeId";
  private static final String VENDORS = "vendors";
  private static final String SYSTEM_FEEDBACK = "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"/path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":true,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"/path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String USER_FEEDBACK = "{\"userFeedback\":[{\"locationPath\":\"/path1\",\"userPrediction\":[\"Text\",\"Blur\"]}]}";
  private static final String USER_FEEDBACK_1 = "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/path1\",\"userPrediction\":[\"Text\",\"Blur\"]},{\"locationPath\":\"/filestore/mta/images/source/path2\",\"userPrediction\":[\"Good\"]}],\"otherModelFeedBack\":null}";
  private static final String USER_FEEDBACK_2 = "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/path2\",\"userPrediction\":[\"Good\"]}],\"otherModelFeedBack\":null}";
  private static final String WATERMARK_PRESENT = "watermarkpresent";
  private static final String NSFW_PRESENT = "nsfwpresent";
  private static final String TEXT = "Text";
  private static final String BLUR = "Blur";
  private static final String WARNING_LABEL = "BliLabelWarning";
  private static final String GOOD = "Good";
  private static final String PATH_1 = "/path1";
  private static final String PATH_2 = "/path2";
  private static final String BUSINESS_PARTNER_CODE_1 = "BUSINESS_PARTNER_CODE_1";
  private static final String BUSINESS_PARTNER_NAME_1 = "BUSINESS_PARTNER_NAME_1";
  private static final String BUSINESS_PARTNER_CODE_2 = "BUSINESS_PARTNER_CODE_2";
  public static final String CM = "CM";
  public static final String CC = "CC";
  public static final int VERSION = 1;
  private static final String FAULTY_IMAGE_TYPE = "blur";
  private static final String PRODUCT = "PRODUCT";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "username";
  private static final String C1_CATEGORY_CODE = "c1-category-code";
  private static final String C1_CATEGORY_NAME = "c1-category-came";

  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private MapResponse mapResponse;
  private MapResponse reviewConfigResponse;
  private Map<String, Object> map;
  private Map<String, Object> reviewConfigMap;

  private DistributionProductDetailRequest distributionProductDetailRequest;
  private TaskHistoryResponse taskHistoryResponse;
  private DistributionProductDetailResponse distributionProductDetailResponse;
  private MasterAttributeResponse masterAttributeResponse;
  private ProductDetailWebResponse productDetailWebResponse;
  private ProductDetailResponse productDetailResponse = new ProductDetailResponse();
  private PageMetaData pageMetaData = new PageMetaData(SIZE, PAGE, TOTAL_RECORDS);
  private PrimaryFilterRequest primaryFilterRequest = new PrimaryFilterRequest();
  private PrimaryFilterWebRequest primaryFilterWebRequest = new PrimaryFilterWebRequest();
  private ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse =
      new ProductBusinessPartnerMapperResponse();
  private List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList = new ArrayList();
  private GdnRestListResponse<ProductBusinessPartnerMapperResponse> responseGdnRestListResponse;
  private ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private NeedRevisionRequest needRevisionRequest;
  private AssigneeResponse assigneeResponse = new AssigneeResponse();
  private BulkDeleteProductRequest bulkDeleteProductRequest;
  private List<AssigneeResponse> assigneeResponseList = new ArrayList();
  private GdnRestListResponse<AssigneeResponse> assigneeResponseGdnRestListResponse;
  private RejectProductVendorRequest rejectProductVendorRequest;
  private VendorAssigneeResponse VendorAssigneeResponse = new VendorAssigneeResponse();
  private List<VendorAssigneeResponse> VendorAssigneeResponseList = new ArrayList();
  private GdnRestListResponse<VendorAssigneeResponse> VendorAssigneeResponseGdnRestListResponse;
  private FilterSummaryRequest summaryFilterRequest = new FilterSummaryRequest();
  private DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
  private List<DistributionProductResponse> distributionProductResponseList = new ArrayList<>();
  private GdnRestListResponse<DistributionProductResponse> distributionProductResponseGdnRestListResponse;
  private VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest = new VendorSummaryFilterWebRequest();
  private VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
  private Map<String, List<String>> reviewerList;
  private MapResponse mapResponse1 ;
  private ProductImageQcFeedbackResponse productImageQcFeedbackResponse;
  private ProductImageQcWebRequest productImageQcWebRequest;
  private GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse;
  private Map<String, ProfileResponse> profileResponseMap;
  private ProfileResponse profileResponse;
  private VendorQuickApprovalRequest vendorQuickApprovalRequest;

  @Mock
  private PDTFeign pdtFeign;

  @Mock
  private BPService bpService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private ProductService productService;

  @Mock
  private PartnersEngineService partnersEngineService;

  @InjectMocks
  private VendorServiceImpl vendorService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ProductMTAWrapper productMTAWrapper;

  @Mock
  private XBulkOutboundService xBulkOutboundService;

  @Captor
  private ArgumentCaptor<BulkDownloadRequest> bulkDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkVendorProductAssignRequest> bulkVendorProductAssignRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcFeedbackRequest> productImageQcFeedbackRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkReviewUploadModel> bulkReviewUploadModelArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    ReflectionTestUtils.setField(vendorService, "predictionTypeToDisplayName",
        ImmutableMap.of("protected_brand_predictions", "Brand mismatch"));
    ReflectionTestUtils.setField(vendorService, "predictionTypeToConfidence",
        ImmutableMap.of("protected_brand_predictions", "70"));
    ReflectionTestUtils.setField(vendorService, "predictionTypeToLabelColourMap",
        ImmutableMap.of("Blur", "BliLabelWarning"));

    taskHistoryResponse = new TaskHistoryResponse();
    taskHistoryResponse.setProductCode(PRODUCT_CODE);
    taskHistoryResponse.setState(DESCRIPTION);
    taskHistoryResponse.setReason(SKU_NOTES);
    primaryFilterWebRequest.builder().assignment(Boolean.TRUE).imagePending(Boolean.TRUE).contentPending(Boolean.TRUE)
        .brandPending(Boolean.TRUE).build();
    BeanUtils.copyProperties(primaryFilterRequest, primaryFilterWebRequest);
    productBusinessPartnerMapperResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    productBusinessPartnerMapperResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    responseGdnRestListResponse =
        new GdnRestListResponse<>(productBusinessPartnerMapperResponseList, pageMetaData, DEFAULT_REQUEST_ID);
    VendorAssigneeResponse.setAssigneeEmailId(DEFAULT_ASSIGNEE_NAME);
    VendorAssigneeResponseList.add(VendorAssigneeResponse);
    VendorAssigneeResponseGdnRestListResponse =
        new GdnRestListResponse<>(VendorAssigneeResponseList, pageMetaData, DEFAULT_REQUEST_ID);
    primaryFilterWebRequest.setTimeFilterWebType("ALL");
    primaryFilterRequest.setTimeFilterType(TimeFilterType.ALL);
    screeningProductBulkActionsWebRequest = ScreeningProductBulkActionsWebRequest.builder()
        .productCodes(Collections.singletonList(PRODUCT_CODE)).assignTo(ASSIGNED_TO).assignedBy(ASSIGNED_BY)
        .correctionReason(CORRECTION_REASON).rejectionReason(REJECT_REASON).additionalNotes(ADDITIONAL_NOTES).build();
    screeningProductBulkActionsRequest = ScreeningProductBulkActionsRequest.builder()
        .productCodes(Collections.singletonList(PRODUCT_CODE)).assignTo(ASSIGNED_TO).assignedBy(ASSIGNED_BY)
        .correctionReason(CORRECTION_REASON).rejectionReason(REJECT_REASON).additionalNotes(ADDITIONAL_NOTES).build();

    distributionProductDetailResponse = new DistributionProductDetailResponse();
    distributionProductDetailResponse.setProductCode(PRODUCT_CODE);

    DistributionProductImageResponse distributionProductImageResponse = new DistributionProductImageResponse();
    distributionProductImageResponse.setLocationPath(LOCATION_PATH);
    distributionProductImageResponse.setMainImage(MAIN_IMAGE);
    List<DistributionProductImageResponse> productImages = new ArrayList<>();
    productImages.add(distributionProductImageResponse);
    DistributionProductItemResponse distributionProductItemResponse = new DistributionProductItemResponse();
    distributionProductItemResponse.setProductItemImages(productImages);
    distributionProductItemResponse.setSkuCode(SKU_CODE);
    List<DistributionProductItemResponse> productItems = new ArrayList<>();
    productItems.add(distributionProductItemResponse);
    distributionProductItemResponse.setProductItemAttributes(Collections.emptyList());
    distributionProductDetailResponse.setProductItems(productItems);

    List<DistributionProductAttributeResponse> productAttributes = new ArrayList<>();
    DistributionProductAttributeResponse distributionProductAttributeResponse =
        new DistributionProductAttributeResponse();
    distributionProductAttributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    distributionProductAttributeResponse.setId(ATTRIBUTE_ID);
    distributionProductAttributeResponse.setValue(VALUE);
    distributionProductAttributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributes.add(distributionProductAttributeResponse);
    distributionProductDetailResponse.setProductAttributes(productAttributes);

    masterAttributeResponse = new MasterAttributeResponse();
    masterAttributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    masterAttributeResponse.setId(ATTRIBUTE_ID);
    masterAttributeResponse.setSkuValue(true);
    masterAttributeResponse.setBasicView(true);
    masterAttributeResponse.setVariantCreatingUI(true);
    masterAttributeResponse.setScreeningMandatory(true);

    productDetailWebResponse = new ProductDetailWebResponse();
    ProductItemWebResponse productItemWebResponse = new ProductItemWebResponse();
    productItemWebResponse.setId(ITEM_ID);
    productItemWebResponse.setGeneratedItemName(ITEM_NAME);
    Set<ProductItemWebResponse> productItem = new HashSet<>();
    productItem.add(productItemWebResponse);
    productDetailWebResponse.setProductItemResponses(productItem);

    productDetailResponse.setName(NAME);
    productDetailResponse.setActivated(true);
    productDetailResponse.setViewable(false);
    productDetailResponse.setUrl(URL);
    Image image = new Image();
    image.setId(ITEM_ID);
    image.setMainImages(MAIN_IMAGE);
    image.setLocationPath(LOCATION_PATH);
    image.setOriginalImage(false);
    Image image1 = new Image();
    image1.setId(ITEM_ID);
    image1.setMainImages(MAIN_IMAGE);
    image1.setLocationPath(LOCATION_PATH_1);
    image1.setOriginalImage(true);
    image1.setActive(false);
    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(imageList);
    productItemResponse.setSkuCode(SKU_CODE);
    Set<ProductItemResponse> productItemResponseSet = new HashSet<>();
    productItemResponseSet.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productItemResponseSet);
    List<String> categories = new ArrayList<>();
    categories.add(CATEGORY);
    productDetailResponse.setCategories(categories);
    productDetailResponse.setCategoriesEnglish(categories);

    distributionProductDetailRequest = new DistributionProductDetailRequest();

    summaryFilterRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    summaryFilterRequest.setAssigneeEmailId(DEFAULT_ASSIGNEE_NAME);
    summaryFilterRequest.setTimeFilterType(TimeFilterType.ALL);
    summaryFilterRequest.setContentPending(Boolean.TRUE);
    summaryFilterRequest.setImagePending(Boolean.TRUE);
    summaryFilterRequest.setVendorCode(VENDOR_CODE);
    summaryFilterRequest.setBrandPending(Boolean.TRUE);

    vendorSummaryFilterWebRequest.setContentPending(Boolean.TRUE);
    vendorSummaryFilterWebRequest.setImagePending(Boolean.TRUE);
    vendorSummaryFilterWebRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    vendorSummaryFilterWebRequest.setAssigneeEmailId(DEFAULT_ASSIGNEE_NAME);
    vendorSummaryFilterWebRequest.setTimeFilterWebType(DEFAULT_TIMEFILTER_TYPE_FOR_ALL);
    vendorSummaryFilterWebRequest.setBrandPending(Boolean.TRUE);

    vendorDetailResponse.setAbleToReject(Boolean.TRUE);
    vendorDetailResponse.setVendorCode(VENDOR_CODE);

    distributionProductResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    distributionProductResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    distributionProductResponse.setProductApproverAssignee(DEFAULT_ASSIGNEE_NAME);
    distributionProductResponse.setState(WorkflowWebState.IN_REVIEW);
    distributionProductResponse.setCurrentVendor(vendorDetailResponse);
    distributionProductResponse.setC1CategoryCode(C1_CATEGORY_CODE);
    distributionProductResponse.setC1CategoryName(C1_CATEGORY_NAME);
    distributionProductResponseList.add(distributionProductResponse);
    distributionProductResponseGdnRestListResponse =
        new GdnRestListResponse<>(distributionProductResponseList, new PageMetaData(), DEFAULT_REQUEST_ID);

    screeningProductBulkActionsWebRequest = new ScreeningProductBulkActionsWebRequest();
    screeningProductBulkActionsWebRequest.setAssignedBy(ASSIGNED_BY);
    screeningProductBulkActionsWebRequest.setAssignTo(ASSIGNED_TO);
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    screeningProductBulkActionsWebRequest.setProductCodes(productCodes);
    screeningProductBulkActionsWebRequest.setRejectionReason(REJECT_REASON);
    screeningProductBulkActionsWebRequest.setCorrectionReason(CORRECTION_REASON);
    screeningProductBulkActionsWebRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsWebRequest.setAllVariants(false);

    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setAssignedBy(ASSIGNED_BY);
    screeningProductBulkActionsRequest.setAssignTo(ASSIGNED_TO);
    screeningProductBulkActionsRequest.setProductCodes(productCodes);
    screeningProductBulkActionsRequest.setRejectionReason(REJECT_REASON);
    screeningProductBulkActionsRequest.setCorrectionReason(CORRECTION_REASON);
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);

    needRevisionRequest = new NeedRevisionRequest();
    needRevisionRequest.setAssignedBy(ASSIGNED_BY);
    needRevisionRequest.setAssignTo(ASSIGNED_TO);
    needRevisionRequest.setProductCodes(productCodes);
    needRevisionRequest.setRejectionReason(REJECT_REASON);
    needRevisionRequest.setCorrectionReason(CORRECTION_REASON);
    needRevisionRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    ProductNotesRequest productNotesRequest = new ProductNotesRequest();
    productNotesRequest.setAllVariants(false);
    needRevisionRequest.setProductNotesRequest(productNotesRequest);

    screeningProductBulkActionsRequest.setAllVariants(false);
    mapResponse = new MapResponse();
    map = new HashMap<>();
    map.put(VENDOR_CODE, 1);
    mapResponse.setMap(map);

    reviewConfigResponse = new MapResponse();
    reviewConfigMap = new HashMap<>();
    reviewConfigMap.put(VENDOR_CODE, 1);
    reviewConfigResponse.setMap(map);

    RejectReasonRequest rejectReason = new RejectReasonRequest();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReason.setProduct(product);
    rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setRejectReasonRequest(rejectReason);
    rejectProductVendorRequest.setNotes(NOTES);

    fileContent = new byte[] {-1, -40, -20, -10};
    reviewerList = new HashMap<>();
    reviewerList.put(TYPE, Collections.singletonList(USER_NAME));

    mapResponse1 = new MapResponse();
    Map<String, Object> map = new HashMap<>();
    map.put(VENDORS, Arrays.asList(new VendorCapacityDTO()));
    mapResponse1.setMap(map);

    productImageQcFeedbackResponse = new ProductImageQcFeedbackResponse();
    productImageQcFeedbackResponse.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackResponse.setUserFeedback(USER_FEEDBACK);
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK);

    productImageQcWebRequest = new ProductImageQcWebRequest();
    productImageQcWebRequest.setProductCode(PRODUCT_CODE);
    List<ImageFeedbackWebRequest> imageFeedbackWebRequests = new ArrayList<>();
    ImageFeedbackWebRequest imageFeedbackWebRequest = new ImageFeedbackWebRequest();
    imageFeedbackWebRequest.setLocationPath(PATH_1);
    imageFeedbackWebRequest.setSystemFeedback(Arrays.asList(WATERMARK_PRESENT, NSFW_PRESENT));
    imageFeedbackWebRequest.setUserFeedback(Arrays.asList(TEXT, BLUR));

    ImageFeedbackWebRequest imageFeedbackWebRequest1 = new ImageFeedbackWebRequest();
    imageFeedbackWebRequest1.setLocationPath(PATH_2);
    imageFeedbackWebRequest1.setUserFeedback(Arrays.asList(GOOD));

    imageFeedbackWebRequests.add(imageFeedbackWebRequest);
    imageFeedbackWebRequests.add(imageFeedbackWebRequest1);
    productImageQcWebRequest.setImageFeedback(imageFeedbackWebRequests);

    RejectProductRequest rejectProductRequest = new RejectProductRequest();
    rejectProductRequest.setProductCode(PRODUCT_CODE);
    rejectProductRequest.setMerchantCommissionType(TYPE);

    RejectReason reason = new RejectReason();
    reason.setProduct(product);
    bulkDeleteProductRequest = new BulkDeleteProductRequest();
    bulkDeleteProductRequest.setCodes(List.of(rejectProductRequest));
    bulkDeleteProductRequest.setRejectReason(reason);
    bulkDeleteProductRequest.setNotes(NOTES);

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

    vendorQuickApprovalRequest =
        VendorQuickApprovalRequest.builder().vendorCode(VENDOR_CODE).productCode(PRODUCT_CODE).Notes(NOTES).build();

    ReflectionTestUtils.setField(vendorService, "gcsUrlPrefix",
        "https://storage.googleapis.com/merchant-prod-image-static/source-image/");
    ReflectionTestUtils.setField(vendorService, "pathPrefix",
        "catalog-image");
  }

  @Test
  public void getBusinessPartnersListTest() throws Exception {
    when(this.pdtFeign.getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(responseGdnRestListResponse);
    List<ProductBusinessPartnerMapperWebResponse> response =
        this.vendorService.getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest);
    Mockito.verify(this.pdtFeign).getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void getBusinessPartnersList_expectException() throws Exception {
    when(this.pdtFeign.getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(null);
    try {
      List<ProductBusinessPartnerMapperWebResponse> response =
          this.vendorService.getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest);
    } catch (ClientException e) {
      Mockito.verify(this.pdtFeign).getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest);
    }
  }

  @Test
  public void getBusinessPartnersList_successSetToFalse() throws Exception {
    responseGdnRestListResponse.setSuccess(Boolean.FALSE);
    when(this.pdtFeign.getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(responseGdnRestListResponse);
    try {
      List<ProductBusinessPartnerMapperWebResponse> response =
          this.vendorService.getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest);
    } catch (ClientException e) {
      Mockito.verify(this.pdtFeign).getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest);
    }
  }

  @Test
  public void getAssigneeListTest() throws Exception {
    when(this.pdtFeign.getAssigneeList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(VendorAssigneeResponseGdnRestListResponse);
    List<AssigneeWebResponse> response =
        this.vendorService.getAssigneeList(PAGE, SIZE, primaryFilterWebRequest);
    Mockito.verify(this.pdtFeign).getAssigneeList(PAGE, SIZE, primaryFilterRequest);
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, response.get(0).getAssigneeEmailId());
  }

  @Test
  public void getAssigneeListTest_expectException() throws Exception {
    when(this.pdtFeign.getAssigneeList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(null);
    try {
      List<AssigneeWebResponse> response =
          this.vendorService.getAssigneeList(PAGE, SIZE, primaryFilterWebRequest);
    } catch (ClientException e) {
      Mockito.verify(this.pdtFeign).getAssigneeList(PAGE, SIZE, primaryFilterRequest);
    }
  }

  @Test
  public void getAssigneeListTest_successSetToFalse() throws Exception {
    VendorAssigneeResponseGdnRestListResponse.setSuccess(Boolean.FALSE);
    when(this.pdtFeign.getAssigneeList(PAGE, SIZE, primaryFilterRequest))
        .thenReturn(VendorAssigneeResponseGdnRestListResponse);
    try {
      List<AssigneeWebResponse> response =
          this.vendorService.getAssigneeList(PAGE, SIZE, primaryFilterWebRequest);
    } catch (ClientException e) {
      Mockito.verify(this.pdtFeign).getAssigneeList(PAGE, SIZE, primaryFilterRequest);
    }
  }

  @Test
  public void getEditedByMerchantTest() {
    when(pdtFeign.getEditedByMerchant(PRODUCT_CODE, VERSION))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(null, null, true, REQUEST_ID, true));
    boolean response = vendorService.getEditedByMerchant(PRODUCT_CODE, VERSION);
    verify(pdtFeign).getEditedByMerchant(PRODUCT_CODE, VERSION);
    assertTrue(response);
  }

  @Test
  public void getEditedByMerchantExceptionTest() {
    when(pdtFeign.getEditedByMerchant(PRODUCT_CODE, VERSION)).thenReturn(null);
    try {
      vendorService.getEditedByMerchant(PRODUCT_CODE, VERSION);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pdtFeign).getEditedByMerchant(PRODUCT_CODE, VERSION);
    }
  }

  @Test
  public void getProductHistoryTest() {
    when(pdtFeign.getProductHistory(PRODUCT_CODE, PAGE, SIZE, false))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(taskHistoryResponse), new PageMetaData(), REQUEST_ID));
    Page<ProductHistoryWebResponse> response = vendorService.getProductHistory(PRODUCT_CODE, PAGE, SIZE);
    verify(pdtFeign).getProductHistory(PRODUCT_CODE, PAGE, SIZE, false);
    assertNotNull(response);
    assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductId());
    assertNull(response.getContent().get(0).getNotes());
    assertEquals(SKU_NAME,
        ((ProductSkuUpdateHistoryWebResponse) response.getContent().get(0).getProductUpdateHistoryWebResponseList()
            .get(1)).getSkuName());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE, response.getContent().get(0).getDescription());
  }

  @Test
  public void getProductHistoryExceptionTest() {
    when(pdtFeign.getProductHistory(PRODUCT_CODE, PAGE, SIZE, false)).thenReturn(null);
    try {
      vendorService.getProductHistory(PRODUCT_CODE, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pdtFeign).getProductHistory(PRODUCT_CODE, PAGE, SIZE, false);
    }
  }

  @Test
  public  void getProductDetailsByProductCodeTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertEquals(CC, responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  void getProductDetailsByProductCodeTest_instoreSellerAndInstoreEnabled() throws Exception {
    ReflectionTestUtils.setField(vendorService, "instoreNewFlowEnabled", true);
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    when(productService.getProductL3BasicDetails(PRODUCT_CODE)).thenReturn(
        ProductL3BasicResponse.builder().pureInStoreProduct(true).build());
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    verify(productService).getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertEquals(CC, responseSingleBaseResponse.getValue().getCommissionType());
    assertTrue(responseSingleBaseResponse.getValue().isPureInstore());
  }

  @Test
  void getProductDetailsByProductCodeTest_nonInstoreSellerAndInstoreEnabled() throws Exception {
    ReflectionTestUtils.setField(vendorService, "instoreNewFlowEnabled", true);
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertEquals(CC, responseSingleBaseResponse.getValue().getCommissionType());
    assertFalse(responseSingleBaseResponse.getValue().isPureInstore());
  }

  @Test
  void getProductDetailsByProductCodeTestInteralProduct() throws Exception {
    ReflectionTestUtils.setField(vendorService, "instoreNewFlowEnabled", true);
    distributionProductDetailResponse.setBusinessPartnerCode(Constants.USER_TYPE_INTERNAL);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertFalse(responseSingleBaseResponse.getValue().isPureInstore());
  }

  @Test
  void getProductDetailsByProductCodeTest_instoreSellerAndInstoreEnabledPbpApiFailed() throws Exception {
    ReflectionTestUtils.setField(vendorService, "instoreNewFlowEnabled", true);
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(profileResponse);
    when(productService.getProductL3BasicDetails(PRODUCT_CODE)).thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    verify(productService).getProductL3BasicDetails(PRODUCT_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertEquals(CC, responseSingleBaseResponse.getValue().getCommissionType());
    assertFalse(responseSingleBaseResponse.getValue().isPureInstore());
  }


  @Test
  public void getProductDetailsByProductCodeTest_NotFound() throws Exception {
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>("", "", false, distributionProductDetailResponse, REQUEST_ID));
    vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
  }

  @Test
  public  void getProductDetailsByProductCodeNullBPCodeTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(null);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService, times(0)).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  public void getProductDetailsByProductCodeXBPExceptionTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  public void getProductDetailsByProductCodeEditedImageTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setEdited(true);
    image.setActive(false);
    image.setOriginalImage(false);
    image.setMainImages(true);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setEdited(true);
    image1.setActive(true);
    image1.setOriginalImage(true);
    image1.setMainImages(true);

    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);

    productDetailResponse.getProductItemResponses().iterator().next().setImages(imageList);

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  public void getProductDetailsByProductCodeEditedImageNoThumbailTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setEdited(true);
    image.setActive(false);
    image.setOriginalImage(false);
    image.setMainImages(false);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setEdited(true);
    image1.setActive(true);
    image1.setOriginalImage(true);
    image1.setMainImages(false);

    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);

    productDetailResponse.getProductItemResponses().iterator().next().setImages(imageList);

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertTrue(StringUtils.isEmpty(productItemWebResponse.getThumbnailPath()));
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  public void getProductDetailsByProductCodeEditedCommonImageTrueImageTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setEdited(true);
    image.setActive(false);
    image.setOriginalImage(false);
    image.setMainImages(true);
    image.setCommonImage(true);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setEdited(true);
    image1.setActive(true);
    image1.setOriginalImage(true);
    image1.setMainImages(true);
    image1.setCommonImage(true);

    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);

    productDetailResponse.getProductItemResponses().iterator().next().setImages(imageList);
    List<Image> imageListProductLevel = new ArrayList<>();
    Image image2 = new Image();
    image2.setCommonImage(true);
    image2.setOriginalImage(true);
    image2.setMainImages(true);
    image2.setLocationPath(LOCATION_PATH);
    image2.setActive(true);

    Image image3 = new Image();
    image3.setCommonImage(false);
    image3.setOriginalImage(false);
    image3.setMainImages(false);
    image3.setLocationPath(LOCATION_PATH_2);
    image3.setActive(true);

    Image image4 = new Image();
    image4.setCommonImage(true);
    image4.setOriginalImage(false);
    image4.setMainImages(true);
    image4.setLocationPath(LOCATION_PATH_3);
    image4.setActive(true);

    Image image5 = new Image();
    image5.setCommonImage(false);
    image5.setOriginalImage(true);
    image5.setMainImages(true);
    image5.setLocationPath(LOCATION_PATH_2);
    image5.setActive(true);

    Image image6 = new Image();
    image6.setCommonImage(true);
    image6.setOriginalImage(true);
    image6.setMainImages(false);
    image6.setLocationPath(LOCATION_PATH_2);
    image6.setActive(true);

    imageListProductLevel.add(image3);
    imageListProductLevel.add(image4);
    imageListProductLevel.add(image5);
    imageListProductLevel.add(image6);
    imageListProductLevel.add(image2);
    productDetailResponse.setImages(imageListProductLevel);

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
    assertTrue(responseSingleBaseResponse.getValue().isCommonImageThumbnailActive());
    assertEquals(LOCATION_PATH, responseSingleBaseResponse.getValue().getCommonImageThumbnailPath());
  }

  @Test
  public void getProductDetailsByProductCodeEditedCommonImageTrueImageTest_npeFix() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setEdited(true);
    image.setActive(false);
    image.setMainImages(true);
    image.setCommonImage(true);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setEdited(true);
    image1.setActive(true);
    image1.setOriginalImage(true);
    image1.setMainImages(true);
    image1.setCommonImage(true);

    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);

    productDetailResponse.getProductItemResponses().iterator().next().setImages(imageList);
    List<Image> imageListProductLevel = new ArrayList<>();
    Image image2 = new Image();
    image2.setCommonImage(true);
    image2.setOriginalImage(true);
    image2.setMainImages(true);
    image2.setLocationPath(LOCATION_PATH);
    image2.setActive(true);

    Image image3 = new Image();
    image3.setCommonImage(false);
    image3.setOriginalImage(false);
    image3.setMainImages(false);
    image3.setLocationPath(LOCATION_PATH_2);
    image3.setActive(true);

    Image image4 = new Image();
    image4.setCommonImage(true);
    image4.setOriginalImage(false);
    image4.setMainImages(true);
    image4.setLocationPath(LOCATION_PATH_3);
    image4.setActive(true);

    Image image5 = new Image();
    image5.setCommonImage(false);
    image5.setOriginalImage(true);
    image5.setMainImages(true);
    image5.setLocationPath(LOCATION_PATH_2);
    image5.setActive(true);

    Image image6 = new Image();
    image6.setCommonImage(true);
    image6.setOriginalImage(true);
    image6.setMainImages(false);
    image6.setLocationPath(LOCATION_PATH_2);
    image6.setActive(true);

    imageListProductLevel.add(image3);
    imageListProductLevel.add(image4);
    imageListProductLevel.add(image5);
    imageListProductLevel.add(image6);
    imageListProductLevel.add(image2);
    productDetailResponse.setImages(imageListProductLevel);

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
    assertTrue(responseSingleBaseResponse.getValue().isCommonImageThumbnailActive());
    assertEquals(LOCATION_PATH, responseSingleBaseResponse.getValue().getCommonImageThumbnailPath());
  }

  @Test
  public void getProductDetailsByProductCodeEditedImageOriginalImageNullTest() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setEdited(true);
    image.setActive(false);
    image.setOriginalImage(false);
    image.setMainImages(true);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH);
    image1.setEdited(true);
    image1.setActive(true);
    image1.setOriginalImage(null);
    image1.setMainImages(true);

    List<Image> imageList = new ArrayList<>();
    imageList.add(image);
    imageList.add(image1);

    productDetailResponse.getProductItemResponses().iterator().next().setImages(imageList);

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null)).
        thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
        .thenReturn(null);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
        vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
        responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());
    List<ProductAttributeWebResponse> productAttributeResponses =
        responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());
    assertNull(responseSingleBaseResponse.getValue().getCommissionType());
  }

  @Test
  public  void getProductDetailsByProductCode_expectException() {
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE)).
        thenThrow(RuntimeException.class);
    try {
      vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    } catch (Exception e) {
    } finally {
      verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    }
  }

  public void updateProductTest_Content() {
    when(pdtFeign.updateProductContent(eq(VENDOR_CODE), any(DistributionProductDetailRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    vendorService.updateProduct(Constants.VENDOR_TYPE_CONTENT, VENDOR_CODE, distributionProductDetailRequest);
    verify(pdtFeign).updateProductContent(VENDOR_CODE, distributionProductDetailRequest);
  }

  @Test
  public void updateProductTest_Image() {
    when(pdtFeign.updateProductImage(eq(VENDOR_CODE), any(DistributionProductDetailRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    vendorService.updateProduct(Constants.VENDOR_TYPE_IMAGE, VENDOR_CODE, distributionProductDetailRequest);
    verify(pdtFeign).updateProductImage(VENDOR_CODE, distributionProductDetailRequest);
  }

  @Test
  public void getFilterCountsTest() {
    when(pdtFeign.getProductFilterInReview(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE))
        .thenReturn(new GdnRestSingleResponse<>(mapResponse, REQUEST_ID));
    MapResponse response = vendorService.getFilterCounts(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
      Boolean.FALSE);
    verify(pdtFeign).getProductFilterInReview(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
      Boolean.FALSE);
    assertEquals(1, response.getMap().get(VENDOR_CODE));
  }

  @Test
  public void getFilterCountsExceptionTest() {
    when(pdtFeign.getProductFilterInReview(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE)).thenReturn(null);
    try {
      vendorService.getFilterCounts(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE, Boolean.FALSE);
    } catch (ClientException e) {
      verify(pdtFeign).getProductFilterInReview(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
        Boolean.FALSE);
    }
  }

  @Test
  public void getReviewConfigCountsTest() {
    when(pdtFeign.getProductReviewConfigCounts(VENDOR_CODE))
        .thenReturn(new GdnRestSingleResponse<>(reviewConfigResponse, REQUEST_ID));
    MapResponse response = vendorService.getReviewConfigProductCounts(VENDOR_CODE);
    verify(pdtFeign).getProductReviewConfigCounts(VENDOR_CODE);
    assertEquals(1, response.getMap().get(VENDOR_CODE));
  }

  @Test
  public void getReviewConfigCountsExceptionTest() {
    when(pdtFeign.getProductReviewConfigCounts(VENDOR_CODE)).thenReturn(null);
    try {
      vendorService.getReviewConfigProductCounts(VENDOR_CODE);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    }  finally {
      verify(pdtFeign).getProductReviewConfigCounts(VENDOR_CODE);
    }
  }

  @Test
  public void updateProductTest_NullResponse() {
    try {
      vendorService.updateProduct(Constants.VENDOR_TYPE_IMAGE, VENDOR_CODE, distributionProductDetailRequest);
    } catch(ClientException ex) {
      verify(pdtFeign).updateProductImage(VENDOR_CODE, distributionProductDetailRequest);
    }
  }

  @Test
  public void updateProductTest_FailureResponse() {
    try {
      vendorService.updateProduct(Constants.VENDOR_TYPE_IMAGE, VENDOR_CODE, distributionProductDetailRequest);
    } catch(ClientException ex) {
      verify(pdtFeign).updateProductImage(VENDOR_CODE, distributionProductDetailRequest);
    }
  }

  @Test
  public void getProductListTest() {
    Mockito.when(this.pdtFeign.getProductList(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(distributionProductResponseGdnRestListResponse);
    when(bpService.getProfileResponseMap(Mockito.anyList())).thenReturn(profileResponseMap);
    Page<DistributionProductWebResponse> productResponsePage =
        this.vendorService.getVendorProductList(PAGE, SIZE, vendorSummaryFilterWebRequest, VENDOR_CODE);
    Mockito.verify(this.pdtFeign).getProductList(PAGE, SIZE, summaryFilterRequest);
    verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, productResponsePage.getContent().get(0).getProductApproverAssignee());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        productResponsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        productResponsePage.getContent().get(0).getBusinessPartnerName());
    Assertions.assertEquals(VENDOR_CODE, productResponsePage.getContent().get(0).getCurrentVendor().getVendorCode());
    Assertions.assertEquals(C1_CATEGORY_CODE,
      productResponsePage.getContent().getFirst().getC1CategoryCode());
    Assertions.assertEquals(C1_CATEGORY_NAME,
      productResponsePage.getContent().getFirst().getC1CategoryName());
  }

  @Test
  public void getProductListNullBPCodeTest() {
    distributionProductResponseGdnRestListResponse.getContent().get(0).setBusinessPartnerCode(null);
    Mockito.when(this.pdtFeign.getProductList(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(distributionProductResponseGdnRestListResponse);
    Page<DistributionProductWebResponse> productResponsePage =
        this.vendorService.getVendorProductList(PAGE, SIZE, vendorSummaryFilterWebRequest, VENDOR_CODE);
    Mockito.verify(this.pdtFeign).getProductList(PAGE, SIZE, summaryFilterRequest);
    verify(bpService, times(1)).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, productResponsePage.getContent().get(0).getProductApproverAssignee());
    Assertions.assertNull(productResponsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        productResponsePage.getContent().get(0).getBusinessPartnerName());
    Assertions.assertEquals(VENDOR_CODE, productResponsePage.getContent().get(0).getCurrentVendor().getVendorCode());
  }

  @Test
  public void getProductListXBPExceptionTest() {
    Mockito.when(this.pdtFeign.getProductList(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(distributionProductResponseGdnRestListResponse);
    when(bpService.getProfileResponseMap(Mockito.anyList())).thenReturn(new HashMap());
    Page<DistributionProductWebResponse> productResponsePage =
        this.vendorService.getVendorProductList(PAGE, SIZE, vendorSummaryFilterWebRequest, VENDOR_CODE);
    Mockito.verify(this.pdtFeign).getProductList(PAGE, SIZE, summaryFilterRequest);
    verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, productResponsePage.getContent().get(0).getProductApproverAssignee());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        productResponsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        productResponsePage.getContent().get(0).getBusinessPartnerName());
    Assertions.assertEquals(VENDOR_CODE, productResponsePage.getContent().get(0).getCurrentVendor().getVendorCode());
  }

  @Test
  public void getProductList_expectClientExceptionTest() {
    Mockito.when(this.pdtFeign.getProductList(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(null);
    try {
      Page<DistributionProductWebResponse> productResponsePage =
          this.vendorService.getVendorProductList(PAGE, SIZE, vendorSummaryFilterWebRequest, VENDOR_CODE);
    } catch(ClientException e) {
    } finally {
      Mockito.verify(this.pdtFeign).getProductList(PAGE, SIZE, summaryFilterRequest);
    }
  }

  @Test
  public void getProductList_successSetToFalseTest() {
    distributionProductResponseGdnRestListResponse.setSuccess(Boolean.FALSE);
    Mockito.when(this.pdtFeign.getProductList(PAGE, SIZE, summaryFilterRequest))
        .thenReturn(distributionProductResponseGdnRestListResponse);
    try {
      this.vendorService.getVendorProductList(PAGE, SIZE, vendorSummaryFilterWebRequest, VENDOR_CODE);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(this.pdtFeign).getProductList(PAGE, SIZE, summaryFilterRequest);
    }
  }

  @Test
  public void vendorProductActionsTest() {
    screeningProductBulkActionsRequest.setAllVariants(Boolean.FALSE);
    when(pdtFeign.doVendorProductActions(ACTION, screeningProductBulkActionsRequest))
        .thenReturn(new GdnBaseRestResponse(REQUEST_ID));
    vendorService.vendorProductActions(ACTION, screeningProductBulkActionsWebRequest);
    verify(pdtFeign).doVendorProductActions(ACTION, screeningProductBulkActionsRequest);
  }

  @Test
  public void vendorProductActionsTest_failureResponse() {
    screeningProductBulkActionsRequest.setAllVariants(Boolean.FALSE);
    when(pdtFeign.doVendorProductActions(ACTION, screeningProductBulkActionsRequest))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      vendorService.vendorProductActions(ACTION, screeningProductBulkActionsWebRequest);
    } catch (ClientException e) {
      verify(pdtFeign).doVendorProductActions(ACTION, screeningProductBulkActionsRequest);
    }
  }

  @Test
  public void approveVendorProductTest() {
    when(pdtFeign.approveVendorProduct(anyString(), any(DistributionProductDetailRequest.class))).thenReturn(
        new GdnBaseRestResponse(true));
    vendorService.approveVendorProduct(VENDOR_CODE, distributionProductDetailRequest);
    verify(pdtFeign).approveVendorProduct(VENDOR_CODE, distributionProductDetailRequest);
  }

  @Test
  public void approveVendorProductTest_FailureResponse() {
    when(pdtFeign.approveVendorProduct(anyString(), any(DistributionProductDetailRequest.class))).thenReturn(null);
    try {
      vendorService.approveVendorProduct(VENDOR_CODE, distributionProductDetailRequest);
    } catch (ClientException ex) {
      verify(pdtFeign).approveVendorProduct(VENDOR_CODE, distributionProductDetailRequest);
    }
  }

  @Test
  public void vendorRejectProductsTest() throws Exception {
    when(pdtFeign.rejectProduct(eq(VENDOR_CODE), any(RejectProductVendorRequest.class))).thenReturn(
        new GdnBaseRestResponse(REQUEST_ID));
    vendorService.rejectProductByVendor(VENDOR_CODE, bulkDeleteProductRequest);
    ArgumentCaptor<RejectProductVendorRequest> captor =
        ArgumentCaptor.forClass(RejectProductVendorRequest.class);
    verify(pdtFeign).rejectProduct(eq(VENDOR_CODE), captor.capture());
    RejectProductVendorRequest capturedRequest = captor.getValue();
    assertEquals(PRODUCT_CODE, capturedRequest.getProductCode());
    assertEquals(NOTES, capturedRequest.getNotes());
    assertNotNull(capturedRequest.getRejectReasonRequest());
    assertEquals(PRODUCT, capturedRequest.getRejectReasonRequest().getProduct().get(0));
  }

  @Test
  public void vendorRejectProductsNullMerchantCommisionTypeTest() throws Exception {
    when(pdtFeign.rejectProduct(eq(VENDOR_CODE), any(RejectProductVendorRequest.class))).thenReturn(
        new GdnBaseRestResponse(REQUEST_ID));
    bulkDeleteProductRequest.getCodes().get(0).setMerchantCommissionType(null);
    vendorService.rejectProductByVendor(VENDOR_CODE, bulkDeleteProductRequest);
    ArgumentCaptor<RejectProductVendorRequest> captor =
        ArgumentCaptor.forClass(RejectProductVendorRequest.class);
    verify(pdtFeign).rejectProduct(eq(VENDOR_CODE), captor.capture());
    RejectProductVendorRequest capturedRequest = captor.getValue();
    assertEquals(PRODUCT_CODE, capturedRequest.getProductCode());
    assertEquals(NOTES, capturedRequest.getNotes());
    assertNotNull(capturedRequest.getRejectReasonRequest());
    assertEquals(PRODUCT, capturedRequest.getRejectReasonRequest().getProduct().get(0));
  }

  @Test
  void vendorRejectProductsTest_failureResponse() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setErrorCode(PRODUCT);
    response.setSuccess(false);
    when(pdtFeign.rejectProduct(eq(VENDOR_CODE), any())).thenReturn(response);
    vendorService.rejectProductByVendor(VENDOR_CODE, bulkDeleteProductRequest);
    ArgumentCaptor<RejectProductVendorRequest> captor =
        ArgumentCaptor.forClass(RejectProductVendorRequest.class);
    verify(pdtFeign).rejectProduct(eq(VENDOR_CODE), captor.capture());
    RejectProductVendorRequest capturedRequest = captor.getValue();
    assertEquals(PRODUCT_CODE, capturedRequest.getProductCode());
    assertEquals(NOTES, capturedRequest.getNotes());
    assertNotNull(capturedRequest.getRejectReasonRequest());
    assertEquals(PRODUCT, capturedRequest.getRejectReasonRequest().getProduct().get(0));
  }

  @Test
  public void getProductScreeningNotesTest() {
    when(pbpFeign.getProductScreeningNotes(PRODUCT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(new SingleValueResponse(NOTES), REQUEST_ID));
    String productScreeningNotes = vendorService.getProductScreeningNotes(PRODUCT_CODE);
    verify(pbpFeign).getProductScreeningNotes(PRODUCT_CODE);
    assertEquals(NOTES, productScreeningNotes);
  }

  @Test
  public void doProductNeedCorrectionTest() {
    when(pdtFeign.doProductNeedCorrection(VENDOR_CODE, needRevisionRequest))
        .thenReturn(new GdnRestSingleResponse<>(new NeedRevisionResponse(), REQUEST_ID));
    vendorService.doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest);
    verify(pdtFeign).doProductNeedCorrection(VENDOR_CODE, needRevisionRequest);
  }

  @Test
  public void doProductNeedCorrectionNotesNotNullTest() {
    getNeedRevisionNotesWebRequest(screeningProductBulkActionsWebRequest);
    when(pdtFeign.doProductNeedCorrection(VENDOR_CODE, needRevisionRequest))
        .thenReturn(new GdnRestSingleResponse<>(new NeedRevisionResponse(), REQUEST_ID));
    vendorService.doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest);
    verify(pdtFeign).doProductNeedCorrection(VENDOR_CODE, needRevisionRequest);
  }

  @Test
  public void doProductNeedCorrectionItmNotesNotNullTest() {
    getNeedRevisionNotesWebRequestWithItemNotes(screeningProductBulkActionsWebRequest);
    when(pdtFeign.doProductNeedCorrection(VENDOR_CODE, needRevisionRequest))
        .thenReturn(new GdnRestSingleResponse<>(new NeedRevisionResponse(), REQUEST_ID));
    vendorService.doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest);
    verify(pdtFeign).doProductNeedCorrection(VENDOR_CODE, needRevisionRequest);
  }

  private void getNeedRevisionNotesWebRequestWithItemNotes(
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    ItemNotesWebRequest itemNotesWebRequest = new ItemNotesWebRequest();
    itemNotesWebRequest.setItemName(ITEM_NAME);
    itemNotesWebRequest.setItemNumber(1);
    itemNotesWebRequest.setItemSku(ITEM_ID);
    itemNotesWebRequest.setSkuCode(SKU_CODE);
    itemNotesWebRequest.setVendorErrorFields(Collections.singletonList(NOTES));
    itemNotesWebRequest.setVendorNotes(Collections.singletonList(ADDITIONAL_NOTES));
    screeningProductBulkActionsWebRequest.setItemNotes(Collections.singletonList(itemNotesWebRequest));
    ItemNotesRequest itemNotesRequest = new ItemNotesRequest();
    itemNotesRequest.setItemName(ITEM_NAME);
    itemNotesRequest.setItemNumber(1);
    itemNotesRequest.setItemSku(ITEM_ID);
    itemNotesRequest.setSkuCode(SKU_CODE);
    itemNotesRequest.setVendorErrorFields(Collections.singletonList(NOTES));
    itemNotesRequest.setVendorNotes(Collections.singletonList(ADDITIONAL_NOTES));
    needRevisionRequest.setItemNotes(Collections.singletonList(itemNotesRequest));
  }

  private void getNeedRevisionNotesWebRequest(ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    screeningProductBulkActionsWebRequest.setAllVariants(true);
    screeningProductBulkActionsWebRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsWebRequest.setImageReason(Collections.singletonList(REJECT_REASON));
    screeningProductBulkActionsWebRequest.setImagesAdditionalNotes(ACTION);
    screeningProductBulkActionsWebRequest.setNeedRevisionType(TYPE);
    screeningProductBulkActionsWebRequest.setVendorErrorFields(Collections.singletonList(FAULTY_IMAGE_TYPE));
    screeningProductBulkActionsWebRequest.setVendorNotes(Collections.singletonList(NOTES_ASSERTION));
    ProductNotesRequest productNotesRequest = new ProductNotesRequest();
    productNotesRequest.setAllVariants(true);
    productNotesRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    productNotesRequest.setImageReason(Collections.singletonList(REJECT_REASON));
    productNotesRequest.setImagesAdditionalNotes(ACTION);
    needRevisionRequest.setNeedRevisionType(TYPE);
    productNotesRequest.setVendorErrorFields(Collections.singletonList(FAULTY_IMAGE_TYPE));
    productNotesRequest.setVendorNotes(Collections.singletonList(NOTES_ASSERTION));
    needRevisionRequest.setProductNotesRequest(productNotesRequest);
  }

  @Test
  public void doProductNeedCorrectionTestExceptionTest() {
    when(pdtFeign.doProductNeedCorrection(VENDOR_CODE, needRevisionRequest))
        .thenReturn(null);
    try {
      vendorService.doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest);
    } catch (ClientException e) {
      verify(pdtFeign).doProductNeedCorrection(VENDOR_CODE, needRevisionRequest);
    }
  }

  @Test
  public void bulkDownloadFilteredVendorProductsTest() {
    vendorSummaryFilterWebRequest.setFaultyType(FAULTY_IMAGE_TYPE);
    vendorSummaryFilterWebRequest.setB2bActivated(true);
    vendorSummaryFilterWebRequest.setB2cActivated(true);
    this.vendorService.bulkDownloadFilteredVendorProducts(Constants.USER_NAME, Constants.VENDOR_CODE,
        vendorSummaryFilterWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
            bulkDownloadRequestArgumentCaptor.capture());
    VendorSummaryDownloadRequest bulkDownloadRequest =
        (VendorSummaryDownloadRequest) bulkDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(bulkDownloadRequest);
    Assertions.assertEquals(Boolean.TRUE, bulkDownloadRequest.getContentPending());
    Assertions.assertEquals(Boolean.TRUE, bulkDownloadRequest.getImagePending());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkDownloadRequest.getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, bulkDownloadRequest.getAssigneeEmailId());
    Assertions.assertEquals(FAULTY_IMAGE_TYPE, bulkDownloadRequest.getFaultyImageType());
    Assertions.assertTrue(bulkDownloadRequest.getBrandPending());
    Assertions.assertTrue(bulkDownloadRequest.getB2bActivated());
    Assertions.assertTrue(bulkDownloadRequest.getB2cActivated());
  }

  @Test
  public void bulkDownloadFilteredVendorProductsEditedTest() {
    vendorSummaryFilterWebRequest.setFaultyType(FAULTY_IMAGE_TYPE);
    vendorSummaryFilterWebRequest.setEdited(true);
    this.vendorService.bulkDownloadFilteredVendorProducts(Constants.USER_NAME, Constants.VENDOR_CODE,
        vendorSummaryFilterWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
            bulkDownloadRequestArgumentCaptor.capture());
    VendorSummaryDownloadRequest bulkDownloadRequest =
        (VendorSummaryDownloadRequest) bulkDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(bulkDownloadRequest);
    Assertions.assertEquals(Boolean.TRUE, bulkDownloadRequest.getContentPending());
    Assertions.assertEquals(Boolean.TRUE, bulkDownloadRequest.getImagePending());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkDownloadRequest.getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, bulkDownloadRequest.getAssigneeEmailId());
    Assertions.assertEquals(FAULTY_IMAGE_TYPE, bulkDownloadRequest.getFaultyImageType());
    Assertions.assertTrue(bulkDownloadRequest.getBrandPending());
    Assertions.assertTrue(bulkDownloadRequest.getEdited());
  }

  @Test
  public void sendProductBackToVendor() {
    Mockito.when(pdtFeign.sendProductBackToVendor(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    vendorService.sendProductBackToVendor(PRODUCT_CODE);
    Mockito.verify(pdtFeign).sendProductBackToVendor(PRODUCT_CODE);
  }

  @Test
  public void saveBulkAssignFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(partnersEngineService.getReviewers()).thenReturn(reviewerList);
    Mockito.when(fileStorageService.uploadFilePath(any(), anyString(), anyString()))
        .thenReturn(PATH);
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.BULK_INTERNAL_PROCESS_CODE_KEY, Long.valueOf(1)), Constants.REQUEST_ID));
    vendorService.saveBulkAssignFile(STORE_ID, VENDOR_CODE, USER_NAME, REQUEST_ID, multipartFile);
    Mockito.verify(pbpFeign).findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    Mockito.verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
    verify(kafkaPublisher).send(eq(DomainEventName.BULK_ASSIGN_VENDOR_PRODUCT), eq(USER_NAME),
        bulkVendorProductAssignRequestArgumentCaptor.capture());
    verify(partnersEngineService).getReviewers();
    Assertions.assertEquals(Constants.VENDOR_BULK_ASSIGN,
        bulkVendorProductAssignRequestArgumentCaptor.getValue().getBulkProcessType());
    Assertions.assertNotNull(new File(PATH + REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(bulkVendorProductAssignRequestArgumentCaptor.getValue());
  }

  @Test
  public void getProductImageQcFeedbackTest() throws IOException {
    when(pdtFeign.getProductImageFeedback(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, productImageQcFeedbackResponse));
    ProductImageQcWebResponse response = vendorService.getProductImageQcFeedback(PRODUCT_CODE);
    verify(pdtFeign).getProductImageFeedback(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(2, response.getImageFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getUserFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().get(1).getSystemFeedback().size());
    Assertions.assertEquals(PATH_1, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(PATH_2, response.getImageFeedback().get(1).getLocationPath());
    Assertions.assertEquals(WATERMARK_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(1));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(1).getSystemFeedback().get(0));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(0).getUserFeedback().get(0));
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getUserFeedback().get(1));
  }

  @Test
  public void updateProductImageQcFeedbackTest() throws ApplicationException, JsonProcessingException {
    when(pdtFeign.updateProductImageFeedback(any(ProductImageQcFeedbackRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    vendorService.updateProductImageQcFeedback(productImageQcWebRequest);
    verify(pdtFeign).updateProductImageFeedback(productImageQcFeedbackRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(USER_FEEDBACK_1, productImageQcFeedbackRequestArgumentCaptor.getValue().getUserFeedback());
    Assertions.assertTrue(productImageQcFeedbackRequestArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
  public void updateProductImageQcFeedbackFeedbackNoUpdatedTest() throws ApplicationException, JsonProcessingException {
    when(pdtFeign.updateProductImageFeedback(any(ProductImageQcFeedbackRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    productImageQcWebRequest.getImageFeedback().remove(0);
    productImageQcWebRequest.getImageFeedback().get(0).setSystemFeedback(Collections.singletonList(GOOD));
    vendorService.updateProductImageQcFeedback(productImageQcWebRequest);
    verify(pdtFeign).updateProductImageFeedback(productImageQcFeedbackRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(USER_FEEDBACK_2, productImageQcFeedbackRequestArgumentCaptor.getValue().getUserFeedback());
    Assertions.assertFalse(productImageQcFeedbackRequestArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
  public void updateProductImageQcFeedbackExceptionTest() throws ApplicationException, JsonProcessingException {
    try {
      Mockito.when(pdtFeign.updateProductImageFeedback(any()))
          .thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));
      vendorService.updateProductImageQcFeedback(productImageQcWebRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(pdtFeign).updateProductImageFeedback(productImageQcFeedbackRequestArgumentCaptor.capture());
      Assertions.assertEquals(PRODUCT_CODE, productImageQcFeedbackRequestArgumentCaptor.getValue().getProductCode());
      Assertions.assertEquals(USER_FEEDBACK_1, productImageQcFeedbackRequestArgumentCaptor.getValue().getUserFeedback());
      Assertions.assertTrue(productImageQcFeedbackRequestArgumentCaptor.getValue().isFeedbackUpdated());
    }
  }

  @Test
  public void getDifferentFaultyTypeTest() {
    ReflectionTestUtils.setField(vendorService, "categoryPredictionEnabled", true);
    when(pbpFeign.getDifferentPredictionType()).thenReturn(new GdnRestListResponse<>(
        (Collections.singletonList(new PredictionTypeResponse(BLUR, BLUR))),new PageMetaData(), REQUEST_ID));
    when(systemParameterProperties.getIgnoreForImageQc()).thenReturn(BLUR);
    when(systemParameterProperties.getNotIgnoreForImageQc()).thenReturn(GOOD);
    List<ImageFaultyTypeWebResponse> response = vendorService.getDifferentFaultyType();
    verify(pbpFeign).getDifferentPredictionType();
    verify(systemParameterProperties).getIgnoreForImageQc();
    verify(systemParameterProperties).getNotIgnoreForImageQc();
    Assertions.assertEquals(4, response.size());
    Assertions.assertEquals(BLUR, response.get(0).getInName());
    Assertions.assertEquals(BLUR, response.get(0).getEnName());
    Assertions.assertEquals(WARNING_LABEL, response.get(0).getLabelColour());
    Assertions.assertEquals(ImageQcConstants.BRAND_EN, response.get(1).getEnName());
    Assertions.assertEquals(ImageQcConstants.BRAND_IN, response.get(1).getInName());
  }

  @Test
  public void getVendorListTest() {
    when(pdtFeign.countDistributionSummaryByFilter(eq(false), eq(true),
        any(DistributionTaskMultipleFilterRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(mapResponse1, REQUEST_ID));
    List<VendorDetailWebResponse> response = vendorService.getVendorList();
    verify(pdtFeign).countDistributionSummaryByFilter(eq(false), eq(true),
        any(DistributionTaskMultipleFilterRequest.class));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void getVendorListWithVendorEmptyListTest() {
    when(pdtFeign.countDistributionSummaryByFilter(eq(false), eq(true),
        any(DistributionTaskMultipleFilterRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(mapResponse, REQUEST_ID));
    List<VendorDetailWebResponse> response = vendorService.getVendorList();
    verify(pdtFeign).countDistributionSummaryByFilter(eq(false), eq(true),
        any(DistributionTaskMultipleFilterRequest.class));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void reindexProductToSolrTest() {
    Mockito.when(this.pdtFeign.deltaReindexPDTProductSolr(PRODUCT_CODE))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    boolean response = vendorService.reindexProductToSolr(PRODUCT_CODE);
    Mockito.verify(this.pdtFeign).deltaReindexPDTProductSolr(PRODUCT_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void checkCountOfUploadsTest() {
    Mockito.when(this.xBulkOutboundService
        .getPendingProcessCount(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING))
        .thenReturn(new UploadProcessCount(1));
    BulkUpdatePendingWebResposne response =
        vendorService.checkCountOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    Mockito.verify(this.xBulkOutboundService)
        .getPendingProcessCount(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    Assertions.assertFalse(response.isBulkUpdateStatusFlag());
  }

  @Test
  public void checkCountOfUploadsNoUploadTest() {
    Mockito.when(this.xBulkOutboundService
        .getPendingProcessCount(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING))
        .thenReturn(new UploadProcessCount(0));
    BulkUpdatePendingWebResposne response =
        vendorService.checkCountOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    Mockito.verify(this.xBulkOutboundService)
        .getPendingProcessCount(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    Assertions.assertTrue(response.isBulkUpdateStatusFlag());
  }

  @Test
  public void getReviewProductCountsForConfigTest() {
    when(pdtFeign.getReviewConfigCount(VENDOR_CODE, Boolean.FALSE))
        .thenReturn(new GdnRestSingleResponse<>(reviewConfigResponse, REQUEST_ID));
    MapResponse response = vendorService.getReviewProductCountsForConfig(VENDOR_CODE, Boolean.FALSE);
    verify(pdtFeign).getReviewConfigCount(VENDOR_CODE, Boolean.FALSE);
    assertEquals(1, response.getMap().get(VENDOR_CODE));
  }

  @Test
  public void republishEditedProductTest() {
    Mockito.when(pdtFeign.republishEditedProduct(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    vendorService.republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(pdtFeign).republishEditedProduct(PRODUCT_CODE);
  }

  @Test
  public void republishEditedProductExceptionTest() {
    Mockito.when(pdtFeign.republishEditedProduct(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(false));
    try {
      vendorService.republishEditedProduct(PRODUCT_CODE);
    } catch (Exception e) {
      Assertions.assertEquals(ClientException.class, e.getClass());
    } finally {
      Mockito.verify(pdtFeign).republishEditedProduct(PRODUCT_CODE);
    }
  }

  @Test
  public void getVendorDetailTest() {
    Mockito.when(pdtFeign.getVendorByCode(VENDOR_CODE))
        .thenReturn(new GdnRestSingleResponse<>(vendorDetailResponse, REQUEST_ID));
    vendorService.getVendorDetail(VENDOR_CODE);
    Mockito.verify(pdtFeign).getVendorByCode(VENDOR_CODE);
    assertEquals(VENDOR_CODE, vendorDetailResponse.getVendorCode());
  }

  @Test
  public void vendorProductQuickApprovalTest() {
    Mockito.when(pdtFeign.quickApproveProduct(vendorQuickApprovalRequest))
        .thenReturn(new GdnRestSingleResponse<>(new VendorQuickApprovalResponse(), REQUEST_ID));
    vendorService.vendorProductQuickApproval(vendorQuickApprovalRequest);
    Mockito.verify(pdtFeign).quickApproveProduct(vendorQuickApprovalRequest);
  }

  @Test
  public void vendorProductQuickApprovalExceptionTest() {
    Mockito.when(pdtFeign.quickApproveProduct(vendorQuickApprovalRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      vendorService.vendorProductQuickApproval(vendorQuickApprovalRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pdtFeign).quickApproveProduct(vendorQuickApprovalRequest);
    }
  }

  @Test
  public void getProductReviewersTest() throws Exception {
    Map<String, List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(Constants.REVIEWERS, Arrays.asList(VENDORS));
    Mockito.when(partnersEngineService.getReviewers()).thenReturn(reviewersMap);
    vendorService.getProductReviewers();
    Mockito.verify(partnersEngineService).getReviewers();
  }

  @Test
  public void autoAssignProductsTest() throws Exception {
    Mockito.when(pbpFeign.findCounterByKey(anyString()))
        .thenReturn(new GdnRestSingleResponse<>(new SequenceResponse(), REQUEST_ID));
    Mockito.when(pdtFeign.saveDefaultSetting(any(VendorDefaultFilterRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    vendorService.autoAssignProducts(
        new VendorAutoConsignmentWebRequest(new VendorAutoAssignmentFilterWebRequest(), new ArrayList<>(),
            VENDOR_CODE, 0, true), STORE_ID, VENDOR_CODE);
    Mockito.verify(pdtFeign).saveDefaultSetting(any(VendorDefaultFilterRequest.class));
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.VENDOR_AUTO_ASSIGNMENT_EVENT), eq(VENDOR_CODE),
            any());
    Mockito.verify(pbpFeign).findCounterByKey(anyString());
  }

  @Test
  public void autoAssignProductsDefaultSettingFalseTest() throws Exception {
    Mockito.when(pbpFeign.findCounterByKey(anyString()))
        .thenReturn(new GdnRestSingleResponse<>(new SequenceResponse(), REQUEST_ID));
    vendorService.autoAssignProducts(
        new VendorAutoConsignmentWebRequest(new VendorAutoAssignmentFilterWebRequest(), new ArrayList<>(),
            VENDOR_CODE, 0, false), STORE_ID, VENDOR_CODE);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.VENDOR_AUTO_ASSIGNMENT_EVENT), eq(VENDOR_CODE),
            any());
    Mockito.verify(pbpFeign).findCounterByKey(anyString());
  }

  @Test
  public void autoAssignProductsPDTExceptionTest() throws Exception {
    Mockito.when(pdtFeign.saveDefaultSetting(any(VendorDefaultFilterRequest.class))).thenReturn(null);
    try {
      vendorService.autoAssignProducts(
          new VendorAutoConsignmentWebRequest(new VendorAutoAssignmentFilterWebRequest(), new ArrayList<>(),
              VENDOR_CODE, 0, true), STORE_ID, VENDOR_CODE);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pdtFeign).saveDefaultSetting(any(VendorDefaultFilterRequest.class));
    }
  }

  @Test
  public void autoAssignProductsPBPExceptionTest() throws Exception {
    Mockito.when(pdtFeign.saveDefaultSetting(any(VendorDefaultFilterRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(pbpFeign.findCounterByKey(anyString())).thenReturn(null);
    try {
      vendorService.autoAssignProducts(
          new VendorAutoConsignmentWebRequest(new VendorAutoAssignmentFilterWebRequest(), new ArrayList<>(),
              VENDOR_CODE, 0, true), STORE_ID, VENDOR_CODE);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pdtFeign).saveDefaultSetting(any(VendorDefaultFilterRequest.class));
      Mockito.verify(pbpFeign).findCounterByKey(anyString());
    }
  }

  @Test
  public void getDefaultSettingFilterTest() throws Exception {
    Mockito.when(pdtFeign.getDefaultSettingFilter(anyString()))
            .thenReturn(new GdnRestSingleResponse<>(new VendorDefaultFilterResponse(), REQUEST_ID));
    vendorService.getDefaultSetting(anyString());
    Mockito.verify(pdtFeign).getDefaultSettingFilter(anyString());
  }

  @Test
  public void getDefaultSettingFilterExceptionTest() throws Exception {
    Mockito.when(pdtFeign.getDefaultSettingFilter(anyString()))
            .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      vendorService.getDefaultSetting(anyString());
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pdtFeign).getDefaultSettingFilter(anyString());
    }
  }

  @Test
  public void checkPendingAssignmentsTest() {
    Mockito.when(xBulkOutboundService.checkPendingFilesForAutoAssignment(anyString(), anyString(), anyString()))
            .thenReturn(new InternalProcessPendingFilesResponse());
    vendorService.checkPendingAssignments(anyString(), anyString(), anyString());
    Mockito.verify(xBulkOutboundService).checkPendingFilesForAutoAssignment(anyString(), anyString(), anyString());
  }

  @Test
  public void saveBulkBrandAuthFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
            FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(fileStorageService.uploadFilePath(any(), anyString(), anyString()))
            .thenReturn(PATH);
    vendorService.saveBulkReviewFile(multipartFile, TYPE, DEFAULT_REQUEST_ID, DEFAULT_STORE_ID, DEFAULT_USERNAME);
    verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
    verify(kafkaPublisher).send(eq(DomainEventName.BULK_REVIEW_UPLOAD_EVENT), anyString(),
            bulkReviewUploadModelArgumentCaptor.capture());
    assertNotNull(new File(PATH + DEFAULT_REQUEST_ID + ORIGINAL_FILENAME));
    assertNotNull(bulkReviewUploadModelArgumentCaptor.getValue());
    assertEquals(TYPE, bulkReviewUploadModelArgumentCaptor.getValue().getBulkProcessType());
    assertEquals(DEFAULT_REQUEST_ID, bulkReviewUploadModelArgumentCaptor.getValue().getRequestId());
    assertEquals(DEFAULT_STORE_ID, bulkReviewUploadModelArgumentCaptor.getValue().getStoreId());
    assertEquals(DEFAULT_USERNAME, bulkReviewUploadModelArgumentCaptor.getValue().getCreatedBy());
  }
  @Test
  public void getProductDetailsByProductCodeTest_withAdditionalSellerFields() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE))
            .thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));
    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null))
            .thenReturn(productDetailResponse);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
            .thenReturn(profileResponse);
    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE)).thenReturn(masterAttributeResponse);
    ProfileResponse profileResponse = new ProfileResponse();

    CompanyDTO company = new CompanyDTO();
    company.setBusinessPartnerName("RENAMED_STORE");
    company.setOfficer("officer@abc.com");
    company.setMerchantType(CC);

    profileResponse.setCompany(company);

    when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
            .thenReturn(profileResponse);
    SingleBaseResponse<ProductDetailWebResponse> responseSingleBaseResponse =
            vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(pdtFeign).getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    verify(productService).findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null);
    verify(productService).getAttributeInfoByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(bpService).getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    assertEquals(PRODUCT_CODE, responseSingleBaseResponse.getValue().getProductCode());
    ProductItemWebResponse productItemWebResponse =
            responseSingleBaseResponse.getValue().getProductItemResponses().stream().findFirst().get();
    assertEquals(productDetailWebResponse.getId(), responseSingleBaseResponse.getValue().getId());
    assertEquals(productDetailWebResponse.getName(), responseSingleBaseResponse.getValue().getName());
    assertEquals(LOCATION_PATH, productItemWebResponse.getThumbnailPath());
    assertEquals(SKU_CODE, productItemWebResponse.getSkuCode());
    assertEquals(MAIN_IMAGE, productItemWebResponse.getImages().get(0).isMainImages());

    List<ProductAttributeWebResponse> productAttributeResponses =
            responseSingleBaseResponse.getValue().getProductAttributeResponses();
    assertEquals(1, productAttributeResponses.size());
    AttributeWebResponse attributeWebResponse = productAttributeResponses.get(0).getAttribute();
    assertEquals(ATTRIBUTE_CODE, attributeWebResponse.getAttributeCode());
    assertEquals(ATTRIBUTE_ID, attributeWebResponse.getId());
    assertTrue(attributeWebResponse.isBasicView());
    assertTrue(attributeWebResponse.isScreeningMandatory());
    assertTrue(attributeWebResponse.isSkuValue());
    assertTrue(attributeWebResponse.isVariantCreatingUI());
    assertFalse(attributeWebResponse.isSearchAble());
    assertFalse(responseSingleBaseResponse.getValue().isInternationalFlag());

    assertEquals(CC, responseSingleBaseResponse.getValue().getCommissionType());
    assertEquals(BUSINESS_PARTNER_CODE_1, responseSingleBaseResponse.getValue().getBusinessPartnerCode());
    assertEquals("RENAMED_STORE", responseSingleBaseResponse.getValue().getBusinessPartnerName());
    assertEquals("officer@abc.com", responseSingleBaseResponse.getValue().getOfficer());
  }

  @Test
  public void getProductDetailsByProductCodeTest_withoutProfileResponse() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);

    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE))
            .thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null))
            .thenReturn(productDetailResponse);

    when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
            .thenReturn(null);

    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE))
            .thenReturn(masterAttributeResponse);

    SingleBaseResponse<ProductDetailWebResponse> response =
            vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);

    assertNull(response.getValue().getBusinessPartnerName());
    assertNull(response.getValue().getOfficer());
  }

  @Test
  public void getProductDetailsByProductCodeTest_withProfileResponseButEmptyCompany() throws Exception {
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);

    when(pdtFeign.getProductDetails(REQUEST_ID, USER_NAME, PRODUCT_CODE))
            .thenReturn(new GdnRestSingleResponse<>(distributionProductDetailResponse, REQUEST_ID));

    when(productService.findDetailByProductCodeAndReplaceCategoryInfo(PRODUCT_CODE, null))
            .thenReturn(productDetailResponse);

    ProfileResponse profileResponse = new ProfileResponse();

    CompanyDTO company = new CompanyDTO();
    company.setInternationalFlag(false);
    company.setBusinessPartnerName(null);
    company.setOfficer(null);

    profileResponse.setCompany(company);

    when(bpService.getProfileResponseByBusinessPartnerCode(BUSINESS_PARTNER_CODE_1))
            .thenReturn(profileResponse);

    when(productService.getAttributeInfoByAttributeCode(ATTRIBUTE_CODE))
            .thenReturn(masterAttributeResponse);

    SingleBaseResponse<ProductDetailWebResponse> response =
            vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);

    assertNull(response.getValue().getBusinessPartnerName());
    assertNull(response.getValue().getOfficer());
    assertNotNull(response.getValue());
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @AfterEach
  public void tearDown() throws IOException {
    verifyNoMoreInteractions(pdtFeign);
    verifyNoMoreInteractions(pbpFeign);
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(this.kafkaPublisher);
    verifyNoMoreInteractions(systemParameterProperties);
    verifyNoMoreInteractions(productMTAWrapper);
    verifyNoMoreInteractions(partnersEngineService);
    FileUtils.deleteDirectory(new File(PATH));
  }
}
