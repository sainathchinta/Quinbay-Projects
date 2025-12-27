package com.gdn.partners.pcu.internal.service.impl.helper;

import static com.gdn.partners.pcu.internal.model.Constants.INTERNAL_STORE_COPY_ACCESSIBILITY;
import static com.gdn.partners.pcu.internal.model.Constants.INTERNAL_UPDATE_BRAND_AUTHORISATION;
import static com.gdn.partners.pcu.internal.model.Constants.INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY;
import static com.gdn.partners.pcu.internal.model.Constants.USER_NAME;
import static org.mockito.Mockito.mockStatic;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
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
import java.util.Set;

import javax.imageio.ImageIO;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.partners.pcu.internal.client.model.request.VendorAutoAssignmentRequest;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ImageQcConstants;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandRejectWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageFeedbackWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.OtherModelFeedBack;
import com.gdn.partners.pcu.internal.web.model.request.PredictionTypeListWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.TimeFilterWebType;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoAssignmentFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import joptsimple.internal.Strings;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class RequestHelperTest {
  private static final String DEFAULT_BRAND_VALUE = "nike";
  private static final String BRAND = "brand";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com.";
  private static final String DEFAULT_PROFILE_BANNER_PATH = "blibli-com.";
  private static final String BRAND_LOGO_PATH = "nike.";
  private static final String BRAND_LOGO_PATH_1 = "nike-";
  private static final String BRAND_BANNER_PATH = "nike.";
  private static final String BRAND_BANNER_PATH_1 = "nike-";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String NA = "NA";
  private static final String STATUS = "ACTIVE";
  private static final String CREATED_DATE = "createdDate";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String KEY = "key";
  private static final String UPDATED_BY = "userName";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";
  private static final String STORE_ID = "storeId";
  private static final String TYPE = "Suspended";
  private static final String REQUEST_ID = "requestId";
  private static final String REASON = "reason";
  private static final String NOTES = "notes";
  private static final String MERCHANT_CODE = "merchant_code";
  private static final String PRODUCT_SKU = "product_sku";
  private static final String PRODUCT_NAME = "product_name";
  private static final String PRODUCT_CODE = "product_code";
  private static final String ACTION = "ACTIVE";
  private static final String PATH = "/path";
  private static final String SORT_DIRECTION = "ASC";
  private static final String BUSINESS_PARTNER_CODE_1 = "businessPartnerCode1";
  private static final String BUSINESS_PARTNER_NAME_1 = "businessPartnerName1";
  private static final String CONFIGURATION = "Configuration";
  private static final String BULK_UPLOAD_CODE = "BULK_UPLOAD_CODE";
  private static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  private static final String START_DATE = "startDate";
  private static final String END_DATE = "endDate";
  private static final String PRODUCT_CODE1 = "PRODUCT_CODE1";
  private static final String PRODUCT_CODE2 = "PRODUCT_CODE2";
  private static final String USER_FEEDBCK = "{\"userFeedback\":[{\"locationPath\":\"locationPath\",\"userPrediction\":[\"Text\",\"Blur\"]}]}";
  private static final String PRODUCT_ID = "product_id";
  private static final String REJECTED_TYPE = "rejectedType";
  private static final String LOCATION_PATH = "/locationPath";
  private static final String IMAGE_FILEPATH = "/MTA-0451836/nike_image_qc_test_full01_sq4wy57o.jpeg";
  private static final String BLUR = "Blur";
  private static final String USER_FEEDBACK = "{\"userFeedback\":[{\"locationPath\":\"https://storage.googleapis.com/merchant-prod-image-static/source-image/catalog-image/locationPath\",\"userPrediction\":[\"Text\"]}],\"otherModelFeedBack\":null}";
  private static final String USER_FEEDBACK_2 = "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/locationPath\",\"userPrediction\":[\"Text\"]}],\"otherModelFeedBack\":[\"Google restriction\"]}";
  private static final String USER_FEEDBACK_1 = "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451836/nike_image_qc_test_full01_sq4wy57o.jpeg\",\"userPrediction\":[\"Text\"]}],\"otherModelFeedBack\":null}";
  private static final String USER_FEEDBACK_3 = "{\"userFeedback\":null,\"otherModelFeedBack\":[\"Google restriction\"]}";
  private static final String GOOGLE_RESTRICTION = "Google restriction";
  private static final String KEYWORD = "KEYWORD";
  private static final String START_DATE_RECAT = "01/01/2021";
  private static final String END_DATE_RECAT = "02/01/2021";
  private static final String TODAY = "today";
  private static final Date START_DATE_STORE_COPY = new Date();
  private static final Date END_DATE_STORE_COPY = new Date();
  private static final String PROCESS_TYPE = "processType";
  public static final String PROCESS_TYPE_SALES_CATEGORY = "SALES_CATEGORY_UPDATE";
  public static final String PROCESS_TYPE_DELETE_BRAND_AUTH = "DELETE_BRAND_AUTHORISATION";
  public static final String PROCESS_TYPE_STORE_COPY = "STORE_COPY";
  private static final String BULK_PRICE_UPDATE_PROCESS_TYPE = "BULK_PRICE_UPDATE";
  public static final String FILE_NAME = "originalFileName.xlsx";
  public static final String SELLER_CODE = "INTERNAL";
  public static final String SELLER_NAME = "sellerName";
  public static final String BIP_REQUEST_CODE = "BIP-0000001";
  public static final String DEFAULT = "DEFAULT";
  private static final String GCS_URL_PREFIX = "https://storage.googleapis.com/merchant-prod-image-static/source-image/";
  private static final String PATH_PREFIX = "catalog-image";


  private ProductSuggestionWebRequest productSuggestionWebRequest;
  private List<ProductSuggestionWebRequest> productSuggestionWebRequests;
  private BrandRejectWebRequest brandRejectWebRequest;
  private ApproveBrandWipWebRequest approveBrandWipWebRequest;
  private VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest;
  private ProductSuspensionFilterRequest productSuspensionFilterRequest;
  private SuspensionProductBulkActionsWebRequest suspensionProductBulkActionsWebRequest;
  private Map<String, List<String>> reviewerList;
  private ProfileResponse profileResponse = new ProfileResponse();
  private ProfileResponse profileResponse1 = new ProfileResponse();
  private List<ProfileResponse> profileResponseList = new ArrayList<>();
  private ConfigurationWebRequest configurationWebRequest;
  private ConfigurationFilterWebRequest configurationFilterWebRequest =
      new ConfigurationFilterWebRequest();
  private CountWebRequest countWebRequest;
  private DistributionFilterWebRequest distributionFilterWebRequest;
  private ProductVendorWebRequest productVendorWebRequest;
  private ProductImageQcWebRequest productImageQcWebRequest;
  private RejectProductWebRequest rejectProductWebRequest;
  private RecatProcessSummaryWebRequest recatProcessSummaryWebRequest;
  private BrandAuthCreateWebRequest brandAuthCreateWebRequest;

  private static MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;
  private SummaryFilterWebRequest summaryFilterWebRequest;
  private MockedStatic<Credential> mockedStatic;
  private String[] accessibilities =
      new String[] {INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY, INTERNAL_STORE_COPY_ACCESSIBILITY,
          INTERNAL_UPDATE_BRAND_AUTHORISATION, Constants.INTERNAL_BULK_PRICE_UPDATE_ACCESSIBILITY};

  @Test
  public void getSummaryFilterRequestTest() {
    ReviewProductsFilterRequest reviewProductsFilterRequest = ReviewProductsFilterRequest.builder().timeFilter(Constants.DEFAULT_FILTER_STATE)
        .statusFilter(Constants.DEFAULT_FILTER_STATE).searchKeyword(StringUtils.EMPTY).build();
    SummaryFilterRequest request = RequestHelper.getSummaryFilterRequest(reviewProductsFilterRequest);
    Assertions.assertNotNull(request);
    Assertions.assertEquals(Constants.DEFAULT_FILTER_STATE, request.getStatusFilter());
    Assertions.assertEquals(Constants.DEFAULT_FILTER_STATE, request.getTimeFilter());
  }

  @Test
  public void getBusinessPartnerFilterTest() {
    ProductSuspensionFilterRequest productSuspensionFilterRequest =
        ProductSuspensionFilterRequest.builder().status(STATUS).searchKeyword(StringUtils.EMPTY)
            .businessPartnerCode(Constants.BUSINESS_PARTNER_CODE).categoryCode(Constants.CATEGORY_CHANGED).build();
    BusinessPartnerFilterRequest request = RequestHelper.getBusinessPartnerFilter(productSuspensionFilterRequest);
    Assertions.assertNotNull(request);
    Assertions.assertEquals(STATUS, request.getStatus());
    Assertions.assertEquals(Constants.CATEGORY_CHANGED, request.getCategory());
  }

  @Test
  public void getBusinessPartnerFilterFromKeywordTest() {
    BusinessPartnerFilterRequest request = RequestHelper.getBusinessPartnerFilterFromKeyword(KEY);
    Assertions.assertNotNull(request);
    Assertions.assertEquals(STATUS, request.getStatus());
    Assertions.assertEquals(SORT_DIRECTION, request.getSortDirection());
    Assertions.assertEquals(KEY, request.getKeywords());
  }

  @Test
  public void toNeedRevisionRequestTest() {
    ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest =
        new ScreeningProductBulkActionsWebRequest();
    RequestHelper.toNeedRevisionRequest(screeningProductBulkActionsWebRequest);
  }

  @BeforeEach
  public void setUp() throws Exception {

    productSuggestionWebRequests = new ArrayList<>();
    productSuggestionWebRequest = new ProductSuggestionWebRequest();
    productSuggestionWebRequest.setName(BRAND);
    productSuggestionWebRequest.setValue(DEFAULT_BRAND_VALUE);
    productSuggestionWebRequests.add(productSuggestionWebRequest);

    brandRejectWebRequest = new BrandRejectWebRequest();
    brandRejectWebRequest.setBrandRequestCode(BRAND_REQUEST_CODE);

    approveBrandWipWebRequest = new ApproveBrandWipWebRequest();
    approveBrandWipWebRequest.setBrandRequestCode(BRAND_REQUEST_CODE);
    approveBrandWipWebRequest.setBrandName(DEFAULT_BRAND_VALUE);

    vendorSummaryFilterWebRequest = new VendorSummaryFilterWebRequest();
    vendorSummaryFilterWebRequest.setTimeFilterWebType(TimeFilterWebType.ALL.name());
    vendorSummaryFilterWebRequest.setIsCnCategory(false);
    vendorSummaryFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    vendorSummaryFilterWebRequest.setAssigneeEmailId(NA);
    vendorSummaryFilterWebRequest.setFaultyType(BLUR);
    vendorSummaryFilterWebRequest.setEdited(true);

    productSuspensionFilterRequest = new ProductSuspensionFilterRequest();
    productSuspensionFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productSuspensionFilterRequest.setCategoryCode(CATEGORY_CODE);
    productSuspensionFilterRequest.setSearchKeyword(KEY);
    productSuspensionFilterRequest.setStatus(STATUS);

    suspensionProductBulkActionsWebRequest = new SuspensionProductBulkActionsWebRequest();
    suspensionProductBulkActionsWebRequest.setAction(ACTION);
    suspensionProductBulkActionsWebRequest.setReason(REASON);
    suspensionProductBulkActionsWebRequest.setNotes(NOTES);

    ProductSuspensionWebRequest productSuspensionWebRequest = new ProductSuspensionWebRequest();
    productSuspensionWebRequest.setMerchantCode(MERCHANT_CODE);
    productSuspensionWebRequest.setProductSku(PRODUCT_SKU);
    productSuspensionWebRequest.setProductName(PRODUCT_NAME);
    suspensionProductBulkActionsWebRequest.setProducts(Arrays.asList(productSuspensionWebRequest));

    reviewerList = new HashMap<>();
    reviewerList.put(TYPE, Collections.singletonList(UPDATED_BY));

    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setBusinessPartnerName(BUSINESS_PARTNER_NAME);

    profileResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    profileResponse1.setCompany(new CompanyDTO());
    profileResponse1.getCompany().setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);

    profileResponseList.add(profileResponse);
    profileResponseList.add(profileResponse1);

    configurationWebRequest = ConfigurationWebRequest.builder().merchantCode(BUSINESS_PARTNER_CODE)
        .categoryCode(CATEGORY_CODE).build();

    configurationFilterWebRequest.setSortOrder(SORT_DIRECTION);
    configurationFilterWebRequest.setReviewConfig(Constants.PRE_LIVE_STATUS);
    configurationFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterWebRequest.setSearchKey(KEY);

    countWebRequest = new CountWebRequest();
    countWebRequest.setStatus(WorkflowState.PASSED.name());

    summaryFilterWebRequest =
        summaryFilterWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .startDate(START_DATE).endDate(END_DATE).keyword(PRODUCT_NAME).status(STATUS).build();
    distributionFilterWebRequest = new DistributionFilterWebRequest();
    distributionFilterWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    distributionFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    distributionFilterWebRequest.setKeyword(PRODUCT_NAME);
    distributionFilterWebRequest.setSortBy(SORT_DIRECTION);
    distributionFilterWebRequest.setRejectedList(Arrays.asList(1));
    distributionFilterWebRequest.setStatusList(Arrays.asList(WorkflowState.PASSED.name(), WorkflowState.PASSED.name()));
    distributionFilterWebRequest.setVendorCodes(Arrays.asList(VENDOR_CODE));

    productVendorWebRequest = new ProductVendorWebRequest();
    productVendorWebRequest.setVendorCode(VENDOR_CODE);
    productVendorWebRequest.setProductCodes(Arrays.asList(PRODUCT_CODE1, PRODUCT_CODE2));

    productImageQcWebRequest = new ProductImageQcWebRequest();
    ImageFeedbackWebRequest imageFeedbackWebRequest = new ImageFeedbackWebRequest();
    List<String> systemFeedback = new ArrayList<>();
    systemFeedback.add(ImageQcConstants.BLUR_PREDICTION);
    systemFeedback.add(ImageQcConstants.TEXT_PREDICTION);

    List<String> userFeedback = new ArrayList<>();
    userFeedback.add(ImageQcConstants.TEXT_PREDICTION);
    imageFeedbackWebRequest.setSystemFeedback(systemFeedback);
    imageFeedbackWebRequest.setUserFeedback(userFeedback);
    imageFeedbackWebRequest.setLocationPath(LOCATION_PATH);
    productImageQcWebRequest.setProductCode(PRODUCT_CODE);
    productImageQcWebRequest.setImageFeedback(Collections.singletonList(imageFeedbackWebRequest));

    rejectProductWebRequest =
        RejectProductWebRequest.builder().isAssignedToVendor(true).rejectedReason(REASON).rejectedType(REJECTED_TYPE)
            .productId(PRODUCT_ID).build();

    recatProcessSummaryWebRequest =
        RecatProcessSummaryWebRequest.builder().requestStartDate(START_DATE_RECAT)
            .requestEndDate(END_DATE_RECAT).keyword(KEYWORD).build();

    String[] accessibilities = {INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY,
        INTERNAL_STORE_COPY_ACCESSIBILITY, INTERNAL_UPDATE_BRAND_AUTHORISATION};
  }

  private void mockStaticMethods(String[] accessibility){
    mockedStatic = Mockito.mockStatic(Credential.class);
    mockedStatic.when(Credential::getAccessibilities).thenReturn(accessibility);
  }

  @Test
  public void toAttributeReqModelListTest() {
    List<AttributeReqModel> response = RequestHelper.toAttributeReqModelList(productSuggestionWebRequests);
    Assertions.assertEquals(BRAND, response.get(0).getName());
    Assertions.assertEquals(DEFAULT_BRAND_VALUE, response.get(0).getValue());
  }

  @Test
  public void toBrandRejectRequestTest() {
    BrandRejectRequest brandRejectRequest = RequestHelper.toBrandRejectRequest(brandRejectWebRequest);
    Assertions.assertNotNull(brandRejectRequest);
    Assertions.assertEquals(BRAND_REQUEST_CODE, brandRejectRequest.getBrandRequestCode());
  }

  @Test
  public void approveBrandWipWebRequestToBrandApproveRequestTest() {
    BrandApproveRequest response =
        RequestHelper.approveBrandWipWebRequestToBrandApproveRequest(approveBrandWipWebRequest, null, null);
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.getBrandRequestCode());
  }

  @Test
  public void approveBrandWipWebRequestToBrandApproveRequestImageTest() throws Exception {
    BrandApproveRequest response = RequestHelper
        .approveBrandWipWebRequestToBrandApproveRequest(approveBrandWipWebRequest, generateMultipartFile(), null);
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.getBrandRequestCode());
    Assertions.assertTrue(response.getBrandLogoPath().contains(BRAND_LOGO_PATH_1));
  }

  @Test
  public void approveBrandWipWebRequestToBrandApproveRequestBannerImageTest() throws Exception {
    BrandApproveRequest response = RequestHelper
        .approveBrandWipWebRequestToBrandApproveRequest(approveBrandWipWebRequest, generateMultipartFile(),
            generateMultipartFile());
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.getBrandRequestCode());
    System.out.println(">> : " + response.getBrandLogoPath());
    Assertions.assertTrue(response.getBrandLogoPath().contains(BRAND_LOGO_PATH_1));
    Assertions.assertTrue(response.getProfileBannerPath().contains(BRAND_BANNER_PATH_1));
  }

  @Test
  public void toBrandApproveRequestTest() {
    BrandApproveRequest response = RequestHelper.toBrandApproveRequest(approveBrandWipWebRequest, null, null);
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.getBrandRequestCode());
  }

  @Test
  public void toBrandApproveRequestImageTest() throws Exception {
    BrandApproveRequest response = RequestHelper
        .toBrandApproveRequest(approveBrandWipWebRequest, generateMultipartFile(), generateMultipartFile());
    Assertions.assertEquals(BRAND_REQUEST_CODE, response.getBrandRequestCode());
    Assertions.assertTrue(response.getBrandLogoPath().contains(BRAND_LOGO_PATH_1));
    Assertions.assertTrue(response.getProfileBannerPath().contains(BRAND_BANNER_PATH_1));
  }

  private MultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, DEFAULT_BRAND_LOGO_PATH, "image/jpg",
        IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
    width = 640;
    height = 320;
    img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }

  @AfterEach
  public void tearDown() throws Exception {
    deleteFolder(DEFAULT_BRAND_LOGO_PATH);
    deleteFolder(DEFAULT_PROFILE_BANNER_PATH);
    deleteFolder(PATH);
    if (mockedStatic != null) {
      mockedStatic.close();
    }
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void toFilterSummaryRequest() {
    FilterSummaryRequest response =
        RequestHelper.toFilterSummaryRequest(vendorSummaryFilterWebRequest, VENDOR_CODE);
    Assertions.assertEquals(CATEGORY_CODE, response.getCategoryCode());
    Assertions.assertEquals(Strings.EMPTY, response.getAssigneeEmailId());
    Assertions.assertEquals(TimeFilterWebType.ALL.name(), response.getTimeFilterType().name());
    Assertions.assertEquals(BLUR, response.getFaultyImageType());
  }

  @Test
  public void toSummaryFilterRequestTest() {
    SummaryFilterRequest summaryFilterRequest = RequestHelper.toSummaryFilterRequest(productSuspensionFilterRequest);
    Assertions.assertEquals(CATEGORY_CODE, summaryFilterRequest.getCategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, summaryFilterRequest.getBusinessPartnerCode());
    Assertions.assertEquals(KEY, summaryFilterRequest.getSearchKeyword());
    Assertions.assertEquals(STATUS, summaryFilterRequest.getSuspensionStatus());
  }

  @Test
  public void toBulkProductSuspensionRequestTest() {
    BulkProductSuspensionRequest bulkProductSuspensionRequest =
        RequestHelper.toBulkProductSuspensionRequest(STORE_ID, FILE, TYPE, REQUEST_ID, UPDATED_BY);
    Assertions.assertEquals(UPDATED_BY, bulkProductSuspensionRequest.getUpdatedBy());
    Assertions.assertEquals(TYPE, bulkProductSuspensionRequest.getActionType());
    Assertions.assertEquals(PRODUCT_LEVEL_3, bulkProductSuspensionRequest.getBulkProcessType());
    Assertions.assertEquals(STORE_ID, bulkProductSuspensionRequest.getStoreId());
    Assertions.assertEquals(FILE, bulkProductSuspensionRequest.getFilePath());
    Assertions.assertEquals(REQUEST_ID, bulkProductSuspensionRequest.getRequestId());
  }

  @Test
  public void toSuspensionProductRequestTest() {
    SuspensionProductRequest suspensionProductRequest =
        RequestHelper.toSuspensionProductRequest(suspensionProductBulkActionsWebRequest);
    Assertions.assertEquals(ACTION, suspensionProductRequest.getAction());
    Assertions.assertEquals(NOTES, suspensionProductRequest.getNotes());
    Assertions.assertEquals(REASON, suspensionProductRequest.getReason());
    Assertions.assertEquals(1, suspensionProductRequest.getProducts().size());
    Assertions.assertEquals(MERCHANT_CODE, suspensionProductRequest.getProducts().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, suspensionProductRequest.getProducts().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_NAME, suspensionProductRequest.getProducts().get(0).getProductName());
  }

  @Test
  public void toBulkVendorProductAssignRequestTest() {
    BulkVendorProductAssignRequest bulkVendorProductAssignRequest =
        RequestHelper.toBulkVendorProductAssignRequest(STORE_ID, FILE, VENDOR_CODE, REQUEST_ID, UPDATED_BY,
            reviewerList, BIP_REQUEST_CODE);
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductAssignRequest.getUpdatedBy());
    Assertions.assertEquals(Constants.VENDOR_BULK_ASSIGN, bulkVendorProductAssignRequest.getBulkProcessType());
    Assertions.assertEquals(reviewerList, bulkVendorProductAssignRequest.getValidUserRoleList());
    Assertions.assertEquals(STORE_ID, bulkVendorProductAssignRequest.getStoreId());
    Assertions.assertEquals(VENDOR_CODE, bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertEquals(FILE, bulkVendorProductAssignRequest.getFilePath());
    Assertions.assertEquals(REQUEST_ID, bulkVendorProductAssignRequest.getRequestId());
    Assertions.assertEquals(BIP_REQUEST_CODE, bulkVendorProductAssignRequest.getInternalProcessRequestCode());
  }

  @Test
  public void toMerchantConfigurationRequestListTest() {
    List<MerchantConfigurationRequest> merchantConfigurationRequestList =
        RequestHelper.toMerchantConfigurationRequestList(profileResponseList);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, merchantConfigurationRequestList.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, merchantConfigurationRequestList.get(0).getBusinessPartnerName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE_1, merchantConfigurationRequestList.get(1).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME_1, merchantConfigurationRequestList.get(1).getBusinessPartnerName());
  }

  @Test
  public void toConfigurationStatusRequestListTest() {
    List<ConfigurationStatusRequest> configurationStatusRequestList =
        RequestHelper.toConfigurationStatusRequestList(Collections.singletonList(configurationWebRequest));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, configurationStatusRequestList.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, configurationStatusRequestList.get(0).getCategoryCode());
  }

  @Test
  public void createDirIfNotExistsTest() {
    RequestHelper.createDirIfNotExists(PATH);
  }

  @Test
  public void toCategoryConfigurationFilterRequestTest() {
    ConfigurationFilterRequest configurationFilterRequest =
        RequestHelper.toConfigurationFilterRequest(configurationFilterWebRequest);
    Assertions.assertEquals(CATEGORY_CODE, configurationFilterRequest.getCategoryCode());
    Assertions.assertEquals(SORT_DIRECTION, configurationFilterRequest.getSortOrder());
    Assertions.assertEquals(KEY, configurationFilterRequest.getSearchKey());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, configurationFilterRequest.getReviewConfig());
  }

  @Test
  public void toBulkConfigurationUpdateRequestTest() {
    BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest =
        RequestHelper.toBulkConfigurationUpdateRequest(STORE_ID, FILE, TYPE, REQUEST_ID, UPDATED_BY);
    Assertions.assertEquals(UPDATED_BY, bulkConfigurationUpdateRequest.getUpdatedBy());
    Assertions.assertEquals(TYPE, bulkConfigurationUpdateRequest.getActionType());
    Assertions.assertEquals(CONFIGURATION, bulkConfigurationUpdateRequest.getBulkProcessType());
    Assertions.assertEquals(STORE_ID, bulkConfigurationUpdateRequest.getStoreId());
    Assertions.assertEquals(FILE, bulkConfigurationUpdateRequest.getFilePath());
    Assertions.assertEquals(REQUEST_ID, bulkConfigurationUpdateRequest.getRequestId());
  }

  @Test
  public void toProductImageQcFeedbackRequestTest() throws JsonProcessingException {
    productImageQcWebRequest.getImageFeedback().get(0)
        .setLocationPath(PATH_PREFIX + productImageQcWebRequest.getImageFeedback().get(0).getLocationPath());
    ProductImageQcFeedbackRequest request =
        RequestHelper.toProductImageQcFeedbackRequest(productImageQcWebRequest, PATH_PREFIX, GCS_URL_PREFIX);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(USER_FEEDBACK, request.getUserFeedback());
  }

  @Test
  public void toProductImageQcFeedbackRequestOtherModelFeedBackTest() throws JsonProcessingException {
    Set<String> userFeedback = new HashSet<>();
    userFeedback.add(GOOGLE_RESTRICTION);
    productImageQcWebRequest.setOtherModelFeedBack(new OtherModelFeedBack(userFeedback));
    ProductImageQcFeedbackRequest request = RequestHelper.toProductImageQcFeedbackRequest(productImageQcWebRequest, PATH_PREFIX, GCS_URL_PREFIX);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(USER_FEEDBACK_2, request.getUserFeedback());
  }

  @Test
  public void toProductImageQcFeedbackRequesImageFeedbackEmptyTest() throws JsonProcessingException {
    Set<String> userFeedback = new HashSet<>();
    userFeedback.add(GOOGLE_RESTRICTION);
    productImageQcWebRequest.setOtherModelFeedBack(new OtherModelFeedBack(userFeedback));
    productImageQcWebRequest.setImageFeedback(new ArrayList<>());
    ProductImageQcFeedbackRequest request = RequestHelper.toProductImageQcFeedbackRequest(productImageQcWebRequest, PATH_PREFIX, GCS_URL_PREFIX);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(USER_FEEDBACK_3, request.getUserFeedback());
  }

  @Test
  public void toProductImageQcFeedbackRequestFilePathTest() throws JsonProcessingException {
    productImageQcWebRequest.getImageFeedback().get(0).setLocationPath(IMAGE_FILEPATH);
    ProductImageQcFeedbackRequest request =
        RequestHelper.toProductImageQcFeedbackRequest(productImageQcWebRequest, PATH_PREFIX, GCS_URL_PREFIX);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertTrue(request.isFeedbackUpdated());
    Assertions.assertEquals(USER_FEEDBACK_1, request.getUserFeedback());
    Assertions.assertNull(request.getSystemFeedback());
  }

  @Test
  public void toMasterDataBulkUpdateRequestTest() {
    MasterDataBulkUpdateRequest masterDataBulkUpdateRequest = RequestHelper
        .toMasterDataBulkUpdateRequest(REQUEST_ID, STORE_ID, PATH, BULK_UPLOAD_CODE, UPDATED_BY, DEFAULT_CLIENT_HOST, BIP_REQUEST_CODE);
    Assertions.assertEquals(UPDATED_BY, masterDataBulkUpdateRequest.getUpdatedBy());
    Assertions.assertEquals(UPDATED_BY, masterDataBulkUpdateRequest.getEmailCC());
    Assertions.assertEquals(UPDATED_BY, masterDataBulkUpdateRequest.getEmailTo());
    Assertions.assertEquals(BULK_UPLOAD_CODE, masterDataBulkUpdateRequest.getBulkProcessCode());
    Assertions.assertEquals(DEFAULT_CLIENT_HOST, masterDataBulkUpdateRequest.getClientHost());
    Assertions.assertEquals(STORE_ID, masterDataBulkUpdateRequest.getStoreId());
    Assertions.assertEquals(PATH, masterDataBulkUpdateRequest.getFilePath());
    Assertions.assertEquals(REQUEST_ID, masterDataBulkUpdateRequest.getRequestId());
  }

  @Test
  public void toDistributionTaskMultipleFilterRequestTest() {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        RequestHelper.toDistributionTaskMultipleFilterRequest(countWebRequest);
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequest.getStatusList().size());
    Assertions.assertEquals(WorkflowState.PASSED.name(),
        distributionTaskMultipleFilterRequest.getStatusList().get(0));
  }

  @Test
  public void toDistributionTaskMultipleFilterRequestTest_TimeFilterType() {
    distributionFilterWebRequest.setTimeFilterType(TODAY);
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        RequestHelper.toDistributionTaskMultipleFilterRequest(distributionFilterWebRequest);
    Assertions.assertTrue(distributionTaskMultipleFilterRequest.getTimeFilterType().equals(TODAY));
  }

  @Test
  public void toProductListRequestTest() {
    ProductListRequest productListRequest = RequestHelper.toProductListRequest(summaryFilterWebRequest);
    Assertions.assertEquals(PRODUCT_NAME, productListRequest.getProductName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productListRequest.getBusinessPartnerCode());
    Assertions.assertEquals(START_DATE, productListRequest.getStartDate());
    Assertions.assertEquals(END_DATE, productListRequest.getEndDate());
  }

  public void toDistributionTaskMultipleFilterRequestFromDistributionFilterWebRequestTest() {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        RequestHelper.toDistributionTaskMultipleFilterRequest(distributionFilterWebRequest);
    Assertions.assertEquals(2, distributionTaskMultipleFilterRequest.getStatusList().size());
    Assertions.assertEquals(WorkflowState.PASSED.name(), distributionTaskMultipleFilterRequest.getStatusList().get(0));
    Assertions.assertEquals(WorkflowState.PASSED.name(),
        distributionTaskMultipleFilterRequest.getStatusList().get(1));
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequest.getVendorCodes().size());
    Assertions.assertEquals(VENDOR_CODE, distributionTaskMultipleFilterRequest.getVendorCodes().get(0));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, distributionTaskMultipleFilterRequest.getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, distributionTaskMultipleFilterRequest.getCategoryCode());
    Assertions.assertEquals(PRODUCT_NAME, distributionTaskMultipleFilterRequest.getProductName());
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequest.getRejectedList().size());
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequest.getRejectedList().get(0).longValue());
  }

  @Test
  public void toProductDistributionTaskRequestTest() {
    ProductDistributionTaskRequest productDistributionTaskRequest =
        RequestHelper.toProductDistributionTaskRequest(productVendorWebRequest);
    Assertions.assertEquals(2, productDistributionTaskRequest.getProductCodes().size());
    Assertions.assertEquals(PRODUCT_CODE1, productDistributionTaskRequest.getProductCodes().get(0));
    Assertions.assertEquals(PRODUCT_CODE2, productDistributionTaskRequest.getProductCodes().get(1));
    Assertions.assertEquals(VENDOR_CODE, productDistributionTaskRequest.getVendorCode());
  }

  @Test
  public void toRejectProductListRequestTest() {
    RejectProductRequest response = RequestHelper.toRejectProductListRequest(rejectProductWebRequest);
    Assertions.assertEquals(PRODUCT_ID, response.getProductId());
    Assertions.assertEquals(REASON, response.getRejectedReason());
    Assertions.assertEquals(REJECTED_TYPE, response.getRejectedType());
  }

  @Test
  public void toProductCenterSummaryRequestTest() {
    ProductCenterSummaryWebRequest productCenterSummaryWebRequest =
        new ProductCenterSummaryWebRequest(CATEGORY_CODE, KEYWORD);
    ProductCenterSummaryRequest productCenterSummaryRequest =
        RequestHelper.toProductCenterSummaryRequest(productCenterSummaryWebRequest);
    Assertions.assertEquals(CATEGORY_CODE, productCenterSummaryRequest.getCategoryCode());
    Assertions.assertEquals(KEYWORD, productCenterSummaryRequest.getKeyword());
  }

  @Test
  public void toSalesCategoryMappingUpdateRequest() {
    SalesCategoryMappingWebRequest salesCategoryMappingWebRequest = SalesCategoryMappingWebRequest.builder()
        .addedCategories(Collections.singletonList(new SalesCategoryWebRequest(CATALOG_CODE, CATEGORY_CODE)))
        .deletedCategories(Collections.singletonList(new SalesCategoryWebRequest(CATALOG_CODE, CATEGORY_CODE)))
        .build();
    SalesCategoryMappingUpdateRequest request = RequestHelper.toSalesCategoryMappingUpdateRequest(
        salesCategoryMappingWebRequest);
    Assertions.assertEquals(CATEGORY_CODE, request.getAddedCategories().get(0).getCategoryCode());
    Assertions.assertEquals(null, request.getEtdNotes());
  }

  @Test
  public void toRecatProductSummaryRequestTest() {
    RecatProductSummaryWebRequest recatProductSummaryWebRequest = new RecatProductSummaryWebRequest(STATUS, KEYWORD);
    RecatProductSummaryRequest recatProductSummaryRequest =
        RequestHelper.toRecatProductSummaryRequest(recatProductSummaryWebRequest);
    Assertions.assertEquals(STATUS, recatProductSummaryRequest.getStatus());
    Assertions.assertEquals(KEYWORD, recatProductSummaryRequest.getKeyword());
  }

  @Test
  public void toRecatProductSummaryRequestTest2() {
    RecatProductSummaryWebRequest recatProductSummaryWebRequest = new RecatProductSummaryWebRequest("IN_PROGRESS", KEYWORD );
    RecatProductSummaryRequest recatProductSummaryRequest =
        RequestHelper.toRecatProductSummaryRequest(recatProductSummaryWebRequest);
    Assertions.assertEquals("PENDING", recatProductSummaryRequest.getStatus());
    Assertions.assertEquals(KEYWORD, recatProductSummaryRequest.getKeyword());
  }

  @Test
  public void toRecatProcessSummaryRequestTest() throws Exception {
    RecatProcessSummaryRequest recatProcessSummaryRequest =
        RequestHelper.toRecatProcessSummaryRequest(recatProcessSummaryWebRequest);
    Assertions.assertEquals(KEYWORD, recatProcessSummaryRequest.getKeyword());
    Assertions.assertNotNull(recatProcessSummaryRequest.getRequestEndDate());
    Assertions.assertNotNull(recatProcessSummaryRequest.getRequestStartDate());
  }

  @Test
  public void toRecatProcessSummaryRequest_emptyDateTest() throws Exception {
    recatProcessSummaryWebRequest = new RecatProcessSummaryWebRequest();
    RecatProcessSummaryRequest recatProcessSummaryRequest =
        RequestHelper.toRecatProcessSummaryRequest(recatProcessSummaryWebRequest);
    Assertions.assertNull(recatProcessSummaryRequest.getRequestEndDate());
    Assertions.assertNull(recatProcessSummaryRequest.getRequestStartDate());
  }

  @Test
  public void toBulkInternalProcessSummaryRequestTest() throws Exception{
    BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest =
        new BulkInternalProcessSummaryWebRequest();
    bulkInternalProcessSummaryWebRequest.setStartDate(START_DATE_STORE_COPY);
    bulkInternalProcessSummaryWebRequest.setEndDate(END_DATE_STORE_COPY);
    bulkInternalProcessSummaryWebRequest.setKeyword(KEYWORD);
    bulkInternalProcessSummaryWebRequest.setProcessType(PROCESS_TYPE);
    bulkInternalProcessSummaryWebRequest.setStatus(STATUS);
    bulkInternalProcessSummaryWebRequest.setSortColumn(CREATED_DATE);
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest =
        RequestHelper.toBulkInternalProcessSummaryRequest(bulkInternalProcessSummaryWebRequest);
    Assertions.assertEquals(PROCESS_TYPE, bulkInternalProcessSummaryRequest.getProcessType());
    Assertions.assertEquals(CREATED_DATE, bulkInternalProcessSummaryRequest.getSortColumn());
  }

  @Test
  public void toBulkInternalProcessSummaryRequestEndDateNullTest() throws Exception {
    BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest =
        new BulkInternalProcessSummaryWebRequest();
    bulkInternalProcessSummaryWebRequest.setStartDate(START_DATE_STORE_COPY);
    bulkInternalProcessSummaryWebRequest.setEndDate(null);
    bulkInternalProcessSummaryWebRequest.setKeyword(KEYWORD);
    bulkInternalProcessSummaryWebRequest.setProcessType(PROCESS_TYPE);
    bulkInternalProcessSummaryWebRequest.setStatus(STATUS);
    bulkInternalProcessSummaryWebRequest.setSortColumn(CREATED_DATE);
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest =
        RequestHelper.toBulkInternalProcessSummaryRequest(bulkInternalProcessSummaryWebRequest);
    Assertions.assertEquals(PROCESS_TYPE, bulkInternalProcessSummaryRequest.getProcessType());
    Assertions.assertEquals(CREATED_DATE, bulkInternalProcessSummaryRequest.getSortColumn());
  }

  @Test
  public void toBulkInternalProcessSummaryRequestStartDateNullTest() throws Exception {
    BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest =
        new BulkInternalProcessSummaryWebRequest();
    bulkInternalProcessSummaryWebRequest.setStartDate(null);
    bulkInternalProcessSummaryWebRequest.setEndDate(END_DATE_STORE_COPY);
    bulkInternalProcessSummaryWebRequest.setKeyword(KEYWORD);
    bulkInternalProcessSummaryWebRequest.setProcessType(PROCESS_TYPE);
    bulkInternalProcessSummaryWebRequest.setStatus(STATUS);
    bulkInternalProcessSummaryWebRequest.setSortColumn(CREATED_DATE);
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest =
        RequestHelper.toBulkInternalProcessSummaryRequest(bulkInternalProcessSummaryWebRequest);
    Assertions.assertEquals(PROCESS_TYPE, bulkInternalProcessSummaryRequest.getProcessType());
    Assertions.assertEquals(CREATED_DATE, bulkInternalProcessSummaryRequest.getSortColumn());
  }

  @Test
  public void checkAccessibilityByProcessTypeSalesCategoryTest() throws Exception {
    mockStaticMethods(accessibilities);
    RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_SALES_CATEGORY);
  }

  @Test
  public void checkAccessibilityByProcessTypeStoreCopyTest() throws Exception {
    mockStaticMethods(accessibilities);
    RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_STORE_COPY);
  }

  @Test
  public void checkAccessibilityByProcessTypeStoreCopyExceptionTest() throws Exception {
    mockStaticMethods(new String[]{});
    try {
      RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_STORE_COPY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void checkAccessibilityByProcessTypeDeleteBrandAuthTest() throws Exception {
    mockStaticMethods(accessibilities);
    RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_DELETE_BRAND_AUTH);
  }

  @Test
  public void checkAccessibilityByProcessTypeBulkPriceUpdateTest() {
    mockStaticMethods(accessibilities);
    RequestHelper.checkAccessibilityByProcessType(BULK_PRICE_UPDATE_PROCESS_TYPE);
  }

  @Test
  public void checkAccessibilityByProcessTypeDeleteBrandAuthExceptionTest() throws Exception {
    mockStaticMethods(new String[] {});
    try {
      RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_DELETE_BRAND_AUTH);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }


  @Test
  public void checkAccessibilityByProcessTypeSalesCategoryExceptionTest() throws Exception {
    mockStaticMethods(new String[] {});
    try {
      RequestHelper.checkAccessibilityByProcessType(PROCESS_TYPE_SALES_CATEGORY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void checkAccessibilityByProcessTypeBulkPriceUpdateExceptionTest() {
    mockStaticMethods(new String[]{});
    try {
      RequestHelper.checkAccessibilityByProcessType(BULK_PRICE_UPDATE_PROCESS_TYPE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getUploadRequestByProcessTypeSalesCategoryTest() {
    BulkInternalProcessUploadRequest response = RequestHelper
        .getUploadRequestByProcessType(PROCESS_TYPE_SALES_CATEGORY, FILE_NAME, SELLER_CODE, SELLER_NAME,
            BIP_REQUEST_CODE);
    Assertions.assertEquals(Constants.USER_TYPE_INTERNAL, response.getSellerCode());
    Assertions.assertEquals(Constants.USER_TYPE_INTERNAL, response.getSellerName());
    Assertions.assertEquals(FILE_NAME, response.getFileName());
  }

  @Test
  public void getUploadRequestByProcessTypeBulkPriceUpdateTest() {
    BulkInternalProcessUploadRequest response = RequestHelper
        .getUploadRequestByProcessType(BULK_PRICE_UPDATE_PROCESS_TYPE, FILE_NAME, SELLER_CODE, SELLER_NAME,
            BIP_REQUEST_CODE);
    Assertions.assertEquals(Constants.USER_TYPE_INTERNAL, response.getSellerCode());
    Assertions.assertEquals(SELLER_NAME, response.getSellerName());
    Assertions.assertEquals(FILE_NAME, response.getFileName());
  }

  @Test
  public void getUploadRequestByProcessTypeStoreCopyTest() {
    BulkInternalProcessUploadRequest response = RequestHelper
        .getUploadRequestByProcessType(PROCESS_TYPE_STORE_COPY, FILE_NAME, SELLER_CODE, SELLER_NAME, BIP_REQUEST_CODE);
    Assertions.assertEquals(SELLER_CODE, response.getSellerCode());
    Assertions.assertEquals(FILE_NAME, response.getFileName());
    Assertions.assertEquals(BIP_REQUEST_CODE, response.getInternalProcessRequestCode());
  }

  @Test
  public void getUploadRequestByProcessTypeDefaultTest() {
    BulkInternalProcessUploadRequest response = RequestHelper
        .getUploadRequestByProcessType(DEFAULT, FILE_NAME, SELLER_CODE, SELLER_NAME,
            BIP_REQUEST_CODE);
    Assertions.assertNull(response);
  }

  @Test
  public void validateInternalUploadExcelTypeTest() {
    RequestHelper.validateInternalUploadExcelType(PROCESS_TYPE_STORE_COPY, FILE_NAME);
  }

  @Test
  public void validateBulkPriceUpdateTypeTest() {
    RequestHelper.validateInternalUploadExcelType(BULK_PRICE_UPDATE_PROCESS_TYPE, FILE_NAME);
  }

  @Test
  public void validateDeleteBrandAuthExcelTypeTest() {
    RequestHelper.validateInternalUploadExcelType(PROCESS_TYPE_DELETE_BRAND_AUTH, FILE_NAME);
  }

  @Test
  public void validateInternalUploadExcelTypeExceptionTest() {
    try {
      RequestHelper.validateInternalUploadExcelType(PROCESS_TYPE_STORE_COPY, FILE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void validateInternalUploadExcelTypeDefaultExceptionTest() {
    try {
      RequestHelper.validateInternalUploadExcelType(PROCESS_TYPE, FILE);
    } catch (IllegalStateException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void validateBrandAuthCreateRequest() throws ApplicationException {
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_REQUEST_CODE).brandName(BRAND)
        .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue()).build();
    RequestHelper.validateBrandAuthRequest(brandAuthCreateWebRequest, false, 0);
  }

  @Test
  public void validateBrandAuthUpdateAuthStartDateExceptionRequest() throws ApplicationException {
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, -1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, 10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
        BrandAuthCreateWebRequest.builder().brandCode(BRAND_REQUEST_CODE).brandName(BRAND)
            .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
            .authorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue()).build();
    RequestHelper.validateBrandAuthRequest(brandAuthCreateWebRequest, true, 0);
  }

  @Test
  public void validateBrandAuthCreateAuthEndDateExceptionRequest() throws ApplicationException {
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, -10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_REQUEST_CODE).brandName(BRAND)
        .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue()).build();
    try {
      RequestHelper.validateBrandAuthRequest(brandAuthCreateWebRequest, false, 0);
    } catch (ConstraintViolationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void toGenericStringListRequestTest() {
    PredictionTypeListWebRequest request = new PredictionTypeListWebRequest();
    request.setPredictionTypeList(Collections.singletonList(BRAND));
    GenericStringListRequest genericStringListRequest =
    RequestHelper.toGenericStringListRequest(request);
    Assertions.assertEquals(BRAND, genericStringListRequest.getStringList().get(0));
  }

  @Test
  public void toVendorAutoAssignmentRequestTest() {
    VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest = new VendorAutoConsignmentWebRequest();
    vendorAutoConsignmentWebRequest.setVendorEmail(VENDOR_CODE);
    vendorAutoConsignmentWebRequest.setRequestedSkuCount(1);
    vendorAutoConsignmentWebRequest.setDefaultSettingsEnabled(true);
    vendorAutoConsignmentWebRequest.setAssigneeList(Arrays.asList(VENDOR_CODE, CATALOG_CODE, CATEGORY_CODE));
    vendorAutoConsignmentWebRequest.setVendorAutoAssignmentFilterWebRequest(
        VendorAutoAssignmentFilterWebRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).postLive(true)
            .timeFilterWebType(TYPE).build());
    VendorAutoAssignmentRequest vendorAutoAssignmentRequest =
        RequestHelper.toVendorAutoAssignmentRequest(vendorAutoConsignmentWebRequest, STORE_ID, new SequenceResponse(),
          VENDOR_CODE);
    Assertions.assertEquals(VENDOR_CODE,  vendorAutoAssignmentRequest.getVendorEmail());
    Assertions.assertEquals(3, vendorAutoAssignmentRequest.getAssigneeList().size());
    Assertions.assertEquals(CATEGORY_CODE, vendorAutoAssignmentRequest.getAssigneeList().get(2));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        vendorAutoAssignmentRequest.getVendorAutoAssignmentFilterRequest().getBusinessPartnerCode());
    Assertions.assertTrue(vendorAutoAssignmentRequest.getVendorAutoAssignmentFilterRequest().getPostLive());
    Assertions.assertEquals(TYPE,
        vendorAutoAssignmentRequest.getVendorAutoAssignmentFilterRequest().getTimeFilterWebType());
  }

  @Test
  public void toBulkRestrictedKeywordUploadModelTest() {
    BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel =
        RequestHelper.toBulkRestrictedKeywordUploadModel(STORE_ID, FILE_NAME, PROCESS_TYPE, REQUEST_ID, USER_NAME);
    Assertions.assertEquals(STORE_ID, bulkRestrictedKeywordUploadModel.getStoreId());
    Assertions.assertEquals(FILE_NAME, bulkRestrictedKeywordUploadModel.getFilePath());
    Assertions.assertEquals(PROCESS_TYPE, bulkRestrictedKeywordUploadModel.getBulkProcessType());
    Assertions.assertEquals(REQUEST_ID, bulkRestrictedKeywordUploadModel.getRequestId());
    Assertions.assertEquals(USER_NAME, bulkRestrictedKeywordUploadModel.getUpdatedBy());
  }

  @Test
  public void toBulkBrandAuthUploadModelTest() {
    BulkBrandAuthUploadModel bulkBrandAuthUploadModel =
        RequestHelper.toBulkBrandAuthUploadModel(STORE_ID, FILE_NAME, PROCESS_TYPE, REQUEST_ID, USER_NAME);
    Assertions.assertEquals(STORE_ID, bulkBrandAuthUploadModel.getStoreId());
    Assertions.assertEquals(FILE_NAME, bulkBrandAuthUploadModel.getFilePath());
    Assertions.assertEquals(PROCESS_TYPE, bulkBrandAuthUploadModel.getBulkProcessType());
    Assertions.assertEquals(REQUEST_ID, bulkBrandAuthUploadModel.getRequestId());
    Assertions.assertEquals(USER_NAME, bulkBrandAuthUploadModel.getCreatedBy());
  }

  @Test
  public void toBulkReviewUploadModelTest() {
    BulkReviewUploadModel bulkReviewUploadModel =
      RequestHelper.toBulkReviewUploadModel(STORE_ID, FILE_NAME, PROCESS_TYPE, REQUEST_ID,
        USER_NAME, VENDOR_CODE);
    Assertions.assertEquals(STORE_ID, bulkReviewUploadModel.getStoreId());
    Assertions.assertEquals(FILE_NAME, bulkReviewUploadModel.getFilePath());
    Assertions.assertEquals(PROCESS_TYPE, bulkReviewUploadModel.getBulkProcessType());
    Assertions.assertEquals(REQUEST_ID, bulkReviewUploadModel.getRequestId());
    Assertions.assertEquals(USER_NAME, bulkReviewUploadModel.getCreatedBy());
    Assertions.assertEquals(VENDOR_CODE, bulkReviewUploadModel.getVendorCode());
  }

  @Test
  public void toDeleteProductRequestTest() {
    DeleteProductRequest deleteProductRequest = RequestHelper.toDeleteProductRequest(USER_NAME,
      DeleteProductWebRequest.builder().productCode(PRODUCT_CODE).productName(PRODUCT_NAME)
        .build());
    Assertions.assertEquals(PRODUCT_CODE, deleteProductRequest.getProductCode());
    Assertions.assertEquals(USER_NAME, deleteProductRequest.getUpdatedBy());
  }

  @Test
  public void validateBrandAuthCreateNullDateRequest() throws ApplicationException {
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_REQUEST_CODE).brandName(BRAND)
        .sellerCode(SELLER_CODE).authStartDate(null).authExpireDate(null)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue()).build();
    RequestHelper.validateBrandAuthRequest(brandAuthCreateWebRequest, false, 0);
  }

  @Test
  public void validateBrandAuthCreateMaxFileRequest() throws ApplicationException {
    List<String> fileName = new ArrayList<>();
    fileName.add(DEFAULT.concat("1"));
    fileName.add(DEFAULT.concat("2"));
    fileName.add(DEFAULT.concat("3"));
    fileName.add(DEFAULT.concat("4"));
    fileName.add(DEFAULT.concat("5"));
    fileName.add(DEFAULT.concat("6"));
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_REQUEST_CODE).brandName(BRAND)
        .sellerCode(SELLER_CODE).authStartDate(null).authExpireDate(null)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue()).documentLinks(fileName)
        .build();
    try {
      RequestHelper.validateBrandAuthRequest(brandAuthCreateWebRequest, false, 0);
    } catch (ConstraintViolationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void toHalalProductsFilterRequest() {
    HalalProductsFilterWebRequest halalProductsFilterWebRequest = new HalalProductsFilterWebRequest();
    HalalProductsFilterRequest request =
        RequestHelper.toHalalProductsFilterRequest(halalProductsFilterWebRequest);
    Assertions.assertEquals(halalProductsFilterWebRequest.getKeyword(), request.getKeyword());
    Assertions.assertEquals(halalProductsFilterWebRequest.getBrands(), request.getBrands());
    Assertions.assertEquals(halalProductsFilterWebRequest.getCategories(), request.getCategories());
    Assertions.assertEquals(halalProductsFilterWebRequest.getCurationStatus(), request.getCurationStatus());
  }
}