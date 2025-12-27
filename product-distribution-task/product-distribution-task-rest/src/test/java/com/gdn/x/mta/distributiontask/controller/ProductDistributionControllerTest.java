package com.gdn.x.mta.distributiontask.controller;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.net.URLDecoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.request.AppealProductRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.AiGeneratedFieldsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.AppealProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.RestrictedKeywordsByFieldVendor;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.controller.util.ProductConverterUtil;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.ProductListRequestDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.ExceptionMsg;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductStatusResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductApprovalStatusPublisherService;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;

/**
 * Created by virajjasani on 17/09/16.
 */
public class ProductDistributionControllerTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String MERCHANT_NAME = "merchant1";
  private static final String VENDOR_CODE = "vCode1";
  private static final String VENDOR_ID = "v1";
  private static final String CURRENT_DATE = "2012-12-12 12:12";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "user";
  private static final int IN_REVIEW_COUNT = 2;
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
	private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-0001";
  private static final String GET_PRODUCT_STATUS_IN_REVIEW = "/productFilterCountInReview";
	private static final String GET_PRODUCT_REVIEW_CONFIG_COUNTS = "/productReviewConfigCounts";
	private static final int PAGE = 0;
	private static final int SIZE = 30;
	private static final int NEW_SIZE = 1000;
  private static final String TODAY = "today";
  private static final String YESTERDAY = "yesterday";
  private static final String TWO_DAYS_AGO = "twoDaysAgo";
  private static final String THREE_UNTIL_FIVE_DAYS_AGO = "threeUntilFiveDaysAgo";
  private static final String MORE_THAN_5_DAYS = "moreThan5Days";
  private static final String PENDING = "pending";
  private static final String CONTENT_PENDING = "contentPending";
  private static final String IMAGE_PENDING = "imagePending";
  private static final String ASSIGNED = "assigned";
  private static final String UNASSIGNED = "UNASSIGNED";
  private static final long VERSION = 10;
	private static final String PRE_LIVE = "preLive";
	private static final String POST_LIVE = "postLive";
	private static final String EDITED = "edited";
	private static final String NEWLY_ADDED = "newlyAdded";
	private static final String ASC = "asc";
	private static final String DESC = "desc";
	private static final String BLUR_PREDICTION = "Blur";
	private static final String BRAND_CODE = "brandCode";
	private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
	private static final String FIELD_NAME = "PRODUCT_NAME";
	private static final String RESTRICTED_KEYWORD = "restrictedKeyword";

	private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private ProductListRequest requestForVendor;
  private ProductListRequestDTO productListRequestDTO;
  private Pageable pageable;
  private Product product;
  private Product product1;
  private Page<Product> productPage;
  private ProductReviewer productReviewer;
  private PrimaryFilterRequest primaryFilterRequest = new PrimaryFilterRequest();
	private PrimaryFilterDTO primaryFilterDTO = new PrimaryFilterDTO();
	private ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse =
			new ProductBusinessPartnerMapperResponse();
	private List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList = new ArrayList<>();
	private GdnRestListResponse<ProductBusinessPartnerMapperResponse>
			productBusinessPartnerMapperResponseGdnRestListResponse;
	private static final String DEFAULT_ASSIGNEE_EMAIL_ID = "assigneeEmailId";
	private List<String> assigneeResponseList = new ArrayList<>();
	private SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
	private Map<String, Object> productInReviewCount;
	private Map<String, Object> productReviewConfigCount;
	private ProductNotesResponse productNotesResponse;
	private Map<String, Object> productReviewCountByConfig;
	private List<String> primaryFilterValues = new ArrayList<>(Arrays
      .asList(TODAY, YESTERDAY, TWO_DAYS_AGO, THREE_UNTIL_FIVE_DAYS_AGO, MORE_THAN_5_DAYS, PENDING, CONTENT_PENDING,
          IMAGE_PENDING, ASSIGNED, UNASSIGNED));
	private RestrictedKeywordsByFieldVendor restrictedKeywordsByFieldVendor =
		RestrictedKeywordsByFieldVendor.builder().fieldIdentifier(FIELD_NAME).keywords(Arrays.asList(RESTRICTED_KEYWORD))
			.build();
	AutoNeedRevisionRequest autoNeedRevisionRequest;
	private ProductRetryStatusUpdate productRetryStatusUpdate = new ProductRetryStatusUpdate();
	private BoostedProductFilterRequest boostedProductFilterRequest = new BoostedProductFilterRequest();
	private ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();
	private AppealProductRequest appealProductRequest = new AppealProductRequest();
	@InjectMocks private ProductDistributionController productDistributionController;

  @Mock private ProductService productService;

  @Mock private VendorService vendorService;

  @Mock
  private ProductConverterUtil productConverterUtil;

  @Mock
  private ApprovedProductPublisherService productPublisherService;

  @Mock
  private ProductWrapperService productWrapperService;

  private ObjectMapper objectMapper = new ObjectMapper();

  @Mock
  private ProductApprovalStatusPublisherService productApprovalStatusPublisherService;

  @Mock
  private ProductReviewerService productReviewerService;

  @Mock
  private ProductImageQcFeedbackService productImageQcFeedbackService;

	@Mock
	private ObjectMapper mapper;
  private MockMvc mockMvc;
  private Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers;

	@AfterEach
	public void tearDown() throws Exception {
		verifyNoMoreInteractions(productApprovalStatusPublisherService);
		verifyNoMoreInteractions(productWrapperService);
		verifyNoMoreInteractions(productImageQcFeedbackService);
		verifyNoMoreInteractions(productReviewerService);
	}

  @BeforeEach public void setUp() throws Exception {
	    MockitoAnnotations.openMocks(this);
	    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productDistributionController).build();
	    Date currentDate =
	        new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(URLDecoder.decode(CURRENT_DATE, "UTF-8"));
	    requestForVendor = new ProductListRequest();
	    requestForVendor.setCategoryCode(CATEGORY_CODE);
	    requestForVendor.setBusinessPartnerName(MERCHANT_NAME);
	    requestForVendor.setEndDate(CURRENT_DATE);
	    requestForVendor.setStartDate(CURRENT_DATE);
	    requestForVendor.setProductCode(PRODUCT_CODE);
	    requestForVendor.setVendorCode(VENDOR_CODE);
	    productListRequestDTO = new ProductListRequestDTO();
	    productListRequestDTO.setCategoryCode(CATEGORY_CODE);
	    productListRequestDTO.setEndDate(currentDate);
	    productListRequestDTO.setStartDate(currentDate);
	    productListRequestDTO.setBusinessPartnerName(MERCHANT_NAME);
	    productListRequestDTO.setVendorCode(VENDOR_CODE);
	    productListRequestDTO.setProductCode(PRODUCT_CODE);
	    productListRequestDTO.setStoreId(STORE_ID);
	    pageable = PageRequest.of(PAGE, SIZE);
	    product = new Product();
	    product.setProductCode(PRODUCT_CODE);
	    product.setBusinessPartnerName(MERCHANT_NAME);
	    product.setCategoryCode(CATEGORY_CODE);
	    product.setVendorId(VENDOR_ID);
	    product.setId("id");
	    List<ProductItemAttribute> productItemAttributeList = new ArrayList<>();
	    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
	    ProductItemAttribute deleteproductItemAttribute = new ProductItemAttribute();
	    deleteproductItemAttribute.setMarkForDelete(true);
	    productItemAttributeList.add(productItemAttribute);
	    productItemAttributeList.add(deleteproductItemAttribute);
	    List<ProductItemImage> productItemImageList = new ArrayList<>();
	    ProductItemImage productItemMainImage = new ProductItemImage();
	    productItemMainImage.setMainImage(true);
	    ProductItemImage productItemImage = new ProductItemImage();
	    productItemImage.setMainImage(false);
	    productItemImage.setMarkForDelete(true);
	    productItemImageList.add(productItemImage);
	    productItemImageList.add(productItemMainImage);
	    List<ProductItem> productItemList = new ArrayList<>();
	    ProductItem productItem = new ProductItem();
	    productItem.setId("id");
	    productItem.setSkuCode("skuCode");
	    productItem.setProductItemAttributes(productItemAttributeList);
	    productItem.setProductItemImages(productItemImageList);
	    ProductItem deleteproductItem = new ProductItem();
	    deleteproductItem.setId("id");
	    deleteproductItem.setSkuCode("skuCode");
	    deleteproductItem.setMarkForDelete(true);
	    productItemList.add(productItem);
	    productItemList.add(deleteproductItem);
	    product.setProductItems(productItemList);
	    product.setState(WorkflowState.PASSED);
	    Vendor vendor =
	        new Vendor.Builder().id("id").name("name").isAbleToReject(false).isQcRequired(true).build();
	    product.setCurrentVendor(vendor);
	    List<Product> productList = new ArrayList<>();
	    productList.add(product);
	    productPage = new PageImpl<Product>(productList);

	    List<VendorProductStatusDTO> listVendorProduct = new ArrayList<VendorProductStatusDTO>();
		listVendorProduct.add(new VendorProductStatusDTO(WorkflowState.IN_REVIEW, 2L));
		listVendorProduct.add(new VendorProductStatusDTO(null, 2L));

		List<ProductBusinessPartnerMapper> content = new ArrayList<ProductBusinessPartnerMapper>();
		ProductBusinessPartnerMapper productBusinessPartnerMapper1 = new ProductBusinessPartnerMapper();
		productBusinessPartnerMapper1.setBusinessPartnerCode(MERCHANT_NAME);
		productBusinessPartnerMapper1.setBusinessPartnerName(MERCHANT_NAME);
		content.add(new ProductBusinessPartnerMapper());
		content.add(new ProductBusinessPartnerMapper());
		content.get(0).setBusinessPartnerCode(MERCHANT_NAME);
		content.get(0).setBusinessPartnerName(MERCHANT_NAME);
		content.get(1).setBusinessPartnerCode(MERCHANT_NAME);
		content.get(1).setBusinessPartnerName(MERCHANT_NAME);
		productBusinessPartnerMappers = new PageImpl<ProductBusinessPartnerMapper>(content);

		Mockito.when(vendorService.findByVendorCode(Mockito.anyString()))
			.thenReturn(vendor);
		Mockito.when(productService.findProductStatusForVendor(Mockito.any(Vendor.class), Mockito.anyString()))
			.thenReturn(listVendorProduct);
		primaryFilterRequest.builder().assignment(Boolean.FALSE).contentPending(Boolean.FALSE).imagePending(Boolean.FALSE)
				.keyword(MERCHANT_NAME).timeFilterType(TimeFilterType.ALL).brandPending(Boolean.FALSE).build();
		primaryFilterDTO.builder().assignment(Boolean.FALSE).contentPending(Boolean.FALSE).imagePending(Boolean.FALSE)
				.keyword(MERCHANT_NAME).timeFilterType(TimeFilterType.ALL).brandPending(Boolean.FALSE).build();
		productBusinessPartnerMapperResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
		productBusinessPartnerMapperResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
		productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
		productBusinessPartnerMapperResponseGdnRestListResponse =
				new GdnRestListResponse<>(productBusinessPartnerMapperResponseList, null, REQUEST_ID);
		assigneeResponseList.add(DEFAULT_ASSIGNEE_EMAIL_ID);

    productInReviewCount = new HashMap<>();
    setPrimaryFilter();
		productReviewConfigCount = new HashMap<>();
		productReviewConfigCount.put(PRE_LIVE, 10);
		productReviewConfigCount.put(POST_LIVE, 10);
		productReviewCountByConfig = new HashMap<>();
		productReviewCountByConfig.put(EDITED, 10);
		productReviewCountByConfig.put(NEWLY_ADDED, 10);
	  autoNeedRevisionRequest = new AutoNeedRevisionRequest();
	  autoNeedRevisionRequest.setProductCode(PRODUCT_CODE);
	  productReviewer =
		  ProductReviewer.builder().approverAssignee(DEFAULT_ASSIGNEE_EMAIL_ID).build();
	  when(this.productReviewerService.findProductReviewerByProductCode(
		  Mockito.anyString())).thenReturn(productReviewer);

		productNotesResponse = new ProductNotesResponse();
		productNotesResponse.setLastModified(Constants.REVISED);
		Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(ProductNotesResponse.class)))
				.thenReturn(productNotesResponse);
  }

  private void setPrimaryFilter() {
    for (String filter : primaryFilterValues) {
      productInReviewCount.put(filter, 1);
    }
  }

	private Product createProduct(boolean vendorNullFlag, boolean productReviewerFlag) {
		List<ProductItemImage> productItemImageList = new ArrayList<>();
		ProductItemImage productItemMainImage = new ProductItemImage();
		productItemMainImage.setMainImage(true);
		productItemMainImage.setOriginalImage(null);
		ProductItemImage productItemImage = new ProductItemImage();
		productItemImage.setMainImage(false);
		productItemImage.setOriginalImage(true);
		productItemImage.setMarkForDelete(true);
		productItemImage.setOriginalImage(true);
		productItemImage.setEdited(false);
		ProductItemImage productItemImage1 = new ProductItemImage();
		productItemImage1.setOriginalImage(true);
		productItemImage1.setMainImage(null);
		productItemImage1.setEdited(false);
		productItemImage1.setMarkForDelete(true);
		ProductItemImage productItemImage2 = new ProductItemImage();
		productItemImage2.setMainImage(null);
		productItemImage2.setOriginalImage(null);
		productItemImageList.add(productItemImage1);
		productItemImageList.add(productItemImage);
		productItemImageList.add(productItemMainImage);
		productItemImageList.add(productItemImage2);
		List<ProductItemAttribute> productItemAttributeList = new ArrayList<>();
		ProductItemAttribute productItemAttribute = new ProductItemAttribute();
		ProductItemAttribute deleteproductItemAttribute = new ProductItemAttribute();
		deleteproductItemAttribute.setMarkForDelete(true);
		productItemAttributeList.add(productItemAttribute);
		productItemAttributeList.add(deleteproductItemAttribute);
		List<ProductItem> productItemList = new ArrayList<>();
		ProductItem productItem = new ProductItem();
		productItem.setId("id");
		productItem.setSkuCode("skuCode");
		productItem.setProductItemAttributes(productItemAttributeList);
		productItem.setProductItemImages(productItemImageList);
		ProductItem deleteproductItem = new ProductItem();
		deleteproductItem.setId("id");
		deleteproductItem.setSkuCode("skuCode");
		deleteproductItem.setMarkForDelete(true);
		productItemList.add(productItem);
		productItemList.add(deleteproductItem);
		List<ProductAttribute> productAttributeList = new ArrayList<>();
		ProductAttribute productAttribute = new ProductAttribute();
		productAttribute.setAttributeCode("attribute_code");
		productAttributeList.add(productAttribute);
		productAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
		List<ProductImage> productImageList = new ArrayList<>();
		ProductImage productImage = new ProductImage();
		productImage.setMainImage(true);
		productImage.setOriginalImage(true);
		productImage.setEdited(false);
		ProductImage deleteproductImage = new ProductImage();
		deleteproductImage.setMarkForDelete(true);
		deleteproductImage.setOriginalImage(false);
		ProductImage productImage1 = new ProductImage();
		productImage1.setMarkForDelete(false);
		productImage1.setOriginalImage(false);
		ProductImage productImage2 = new ProductImage();
		productImage2.setMarkForDelete(true);
		productImage2.setOriginalImage(null);
		productImageList.add(productImage);
		productImageList.add(productImage1);
		productImageList.add(deleteproductImage);
		productImageList.add(productImage2);
		Product product =
				new Product.Builder().productName("productName").productCode("code").productAttributes(productAttributeList)
						.productImages(productImageList).productItems(productItemList).build();
		ProductReviewer productReviewer = new ProductReviewer();
		productReviewer.setAssignedDate(new Date());
		productReviewer.setApprovedDate(new Date());
		Vendor currentVendor = new Vendor();
		currentVendor.setName("Vendor1");
		currentVendor.setStoreId("10001");
		currentVendor.setUpdatedDate(new Date());
		currentVendor.setMarkForDelete(false);
		currentVendor.setAbleToReject(false);
		currentVendor.setQcRequired(false);
		currentVendor.setSlaInDays(10);
		currentVendor.setId("31hj134-jbeeqw8-jkdsa2-ksdq234");
		product.setCurrentVendor(vendorNullFlag ? currentVendor : null);
		product.setImageViolations(BLUR_PREDICTION);
		product.setBrandCode(BRAND_CODE);
		product.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
		product.setState(WorkflowState.PASSED);
		product.setEdited(false);
		product.setRestrictedKeywordsDetected("[{\"fieldIdentifier\": \"Product Name\",\"keywords\": [\"alcohol\"]},"
				+ "{\"fieldIdentifier\": \"Description\",\"keywords\": [\"alcohol\"]},{\"fieldIdentifier\": \"USP\","
				+ "\"keywords\": [\"alcohol\"]},{\"fieldIdentifier\": \"WA-0000002\",\"keywords\": [\"alcohol\"]}]");
		product.setProductNotes(
				"{\"vendorNotes\":[\"test\"],\"vendorErrorFields\":[],\"contentAdditionalNotes\":\"\",\"allVariants\":false,"
						+ "\"imagesAdditionalNotes\":\"\",\"imageReason\":\"\",\"lastModified\":\"REVISED\","
						+ "\"modifiedFields\":[\"productName\"]}");
		return product;
	}

	@Test
	public void getSummaryByMultipleFilterListTest() throws Exception {
		DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
				new DistributionTaskMultipleFilterRequest();
		distributionTaskMultipleFilterRequest.setRejectedList(Arrays.asList(1,2));
		distributionTaskMultipleFilterRequest.setVendorCodes(Arrays.asList(VENDOR_CODE));
		distributionTaskMultipleFilterRequest.setStartDate(CURRENT_DATE);
		distributionTaskMultipleFilterRequest.setEndDate(CURRENT_DATE);
		productPage.getContent().get(0).setState(WorkflowState.PASSED);
		String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
		Mockito.when(this.productService
				.getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
						(Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
		Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
		List<WorkflowState> workflowStateList = new ArrayList<>();
		workflowStateList.add(WorkflowState.PASSED);
		workflowStateMap.put("id", workflowStateList);
		Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
				.thenReturn(workflowStateMap);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
								+ ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
						.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
						.param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
						.param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
				.andExpect(jsonPath("$.success", Matchers.equalTo(true)));
		Mockito.verify(this.productService, Mockito.times(1))
				.getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
						(Pageable) Mockito.any(), Mockito.anyString());
		Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
		Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.any(), Mockito.any());
	}


  @Test public void getProductDetailsFalseTest() throws Exception {
		Product product = createProduct(false, false);
	    product.setPostLive(true);
		product.setForceReview(true);
		product.setReviewType(ReviewType.CONTENT);
		product.setEdited(true);
		product.getProductItems().get(0).setNewlyAdded(true);
		product.getProductImages().get(0).setEdited(true);
		product.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
		product.setProductCreationType("PRODUCT_CREATION_TYPE");
		String AI_GENERATED_FIELDS_RESPONSE = "{\"aiGeneratedBrand\":true,\"aiGeneratedCategory\":true}";
		product.setAiGeneratedFields(AI_GENERATED_FIELDS_RESPONSE);
		Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(product);
		Mockito.when(mapper.readValue(anyString(), eq(AiGeneratedFieldsResponse.class))).thenReturn(
				AiGeneratedFieldsResponse.builder().aiGeneratedBrand(true).aiGeneratedCategory(true)
						.build());
	  Mockito.when(mapper.readValue(eq(product.getRestrictedKeywordsDetected()), Mockito.any(TypeReference.class)))
		  .thenReturn(Collections.singletonList(restrictedKeywordsByFieldVendor));
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails = productDistributionController
				.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
		Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
      verify(this.productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
	  Mockito.verify(this.mapper)
		  .readValue(eq(product.getRestrictedKeywordsDetected()), Mockito.any(TypeReference.class));
    Assertions.assertEquals(5, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
						.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertTrue(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
				productDetails.getValue().getBrandApprovalStatus());
		Assertions.assertEquals(WorkflowWebState.PASSED, productDetails.getValue().getState());
		Assertions.assertTrue(productDetails.getValue().isEdited());
		Assertions.assertTrue(productDetails.getValue().getProductImages().get(0).isEdited());
		Assertions.assertTrue(
				productDetails.getValue().getProductItems().get(0).getProductItemImages().get(0).isEdited());
	  Assertions.assertEquals(FIELD_NAME,
				productDetails.getValue().getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
	  Assertions.assertEquals(RESTRICTED_KEYWORD,
				productDetails.getValue().getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
		Assertions.assertTrue(productDetails.getValue().getProductItems().get(0).isNewlyAdded());
		Assertions.assertTrue(
				productDetails.getValue().getAiGeneratedFieldsResponse().isAiGeneratedBrand());
		Assertions.assertTrue(
				productDetails.getValue().getAiGeneratedFieldsResponse().isAiGeneratedCategory());
		Assertions.assertEquals("PRODUCT_CREATION_TYPE",
				productDetails.getValue().getProductCreationType());
  }

	@Test public void getProductDetailsTest() throws Exception {
		Product product = createProduct(true, true);
		product.setEdited(true);
		product.getProductImages().get(0).setEdited(true);
		product.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
		product.getProductItems().get(0).setMinPrice(100.0);
		product.getProductItems().get(0).setMaxPrice(200.0);
		product.getProductItems().get(0).setItemSku("ITEM_SKU");
		product.setPostLive(true);
		product.setForceReview(false);
		Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE))
				.thenReturn(product);
		Mockito.when(mapper.readValue(eq(product.getRestrictedKeywordsDetected()), Mockito.any(TypeReference.class)))
				.thenReturn(Collections.singletonList(restrictedKeywordsByFieldVendor));
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails = productDistributionController
				.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
		Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
        verify(this.productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
		Mockito.verify(this.mapper)
				.readValue(eq(product.getRestrictedKeywordsDetected()), Mockito.any(TypeReference.class));
		Mockito.verify(productConverterUtil).getOrderedTextAndImageViolations(product, null);
		Assertions.assertEquals(5, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
						.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertTrue(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
				productDetails.getValue().getBrandApprovalStatus());
		Assertions.assertEquals(WorkflowWebState.PASSED, productDetails.getValue().getState());
		Assertions.assertTrue(productDetails.getValue().isEdited());
		Assertions.assertTrue(productDetails.getValue().getProductImages().get(0).isEdited());
		Assertions.assertTrue(
				productDetails.getValue().getProductItems().get(0).getProductItemImages().get(0).isEdited());
		Assertions.assertEquals(FIELD_NAME,
				productDetails.getValue().getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
		Assertions.assertEquals(RESTRICTED_KEYWORD,
				productDetails.getValue().getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
		Assertions.assertNotNull(productDetails.getValue().getProductItems().get(0).getMinPrice());
		Assertions.assertEquals("ITEM_SKU",
				productDetails.getValue().getProductItems().get(0).getItemSku());
		Assertions.assertTrue(productDetails.getValue().isShowProductUrl());
	}

	@Test
	public void getProductDetailsSwitchOnTest() throws Exception {
		ReflectionTestUtils.setField(productDistributionController, "overrideMainImageFlag", true);
		Product product = createProduct(true, true);
		product.setEdited(true);
		product.getProductImages().get(0).setEdited(true);
		product.getProductImages().forEach(productImage -> {
			productImage.setMainImage(false);
			productImage.setOriginalImage(false);
			productImage.setCommonImage(true);
		});
		product.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
		product.getProductItems().get(0).setMinPrice(100.0);
		product.getProductItems().get(0).setMaxPrice(200.0);
		product.getProductItems().get(0).setItemSku("ITEM_SKU");
		product.setPostLive(true);
		product.setForceReview(false);
		Mockito.when(
				productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE))
			.thenReturn(product);
		Mockito.when(mapper.readValue(eq(product.getRestrictedKeywordsDetected()),
				Mockito.any(TypeReference.class)))
			.thenReturn(Collections.singletonList(restrictedKeywordsByFieldVendor));
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails =
			productDistributionController.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID,
				REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
		Mockito.verify(productWrapperService)
			.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
		verify(this.productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
		Mockito.verify(this.mapper).readValue(eq(product.getRestrictedKeywordsDetected()),
			Mockito.any(TypeReference.class));
		Mockito.verify(productConverterUtil).getOrderedTextAndImageViolations(product, null);
		Assertions.assertEquals(4, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
			productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
			productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
				.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertTrue(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
			productDetails.getValue().getBrandApprovalStatus());
		Assertions.assertEquals(WorkflowWebState.PASSED, productDetails.getValue().getState());
		Assertions.assertTrue(productDetails.getValue().isEdited());
		Assertions.assertTrue(productDetails.getValue().getProductImages().get(0).isEdited());
		Assertions.assertTrue(
			productDetails.getValue().getProductItems().get(0).getProductItemImages().get(0)
				.isEdited());
		Assertions.assertEquals(FIELD_NAME,
			productDetails.getValue().getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
		Assertions.assertEquals(RESTRICTED_KEYWORD,
			productDetails.getValue().getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
		Assertions.assertNotNull(productDetails.getValue().getProductItems().get(0).getMinPrice());
		Assertions.assertEquals("ITEM_SKU",
			productDetails.getValue().getProductItems().get(0).getItemSku());
		Assertions.assertTrue(productDetails.getValue().isShowProductUrl());
	}

	@Test
	public void getProductDetails_restrictedKeyWordDetectedNullTest() throws Exception {
		Product product = createProduct(true, true);
		product.setEdited(true);
		product.getProductImages().get(0).setEdited(true);
		product.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
		product.setRestrictedKeywordsDetected(null);
		product.setProductNotes(StringUtils.EMPTY);
		Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE))
				.thenReturn(product);
		Mockito.when(mapper.readValue(eq(product.getRestrictedKeywordsDetected()), Mockito.any(TypeReference.class)))
			.thenReturn(Collections.singletonList(restrictedKeywordsByFieldVendor));
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails =
			productDistributionController.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10,
				PRODUCT_CODE);
		Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
        verify(this.productReviewerService).findProductReviewerByProductCode(Mockito.anyString());
		Assertions.assertEquals(5, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
					.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertTrue(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
				productDetails.getValue().getBrandApprovalStatus());
		Assertions.assertEquals(WorkflowWebState.PASSED, productDetails.getValue().getState());
		Assertions.assertTrue(productDetails.getValue().isEdited());
		Assertions.assertTrue(productDetails.getValue().getProductImages().get(0).isEdited());
		Assertions.assertTrue(
				productDetails.getValue().getProductItems().get(0).getProductItemImages().get(0).isEdited());
		Assertions.assertNull(productDetails.getValue().getRestrictedKeywordsDetected());
	}

	@Test public void getProductDetailsTest_nullState() throws Exception {
  	Product product = createProduct(true,true);
  	product.setState(null);
		product.setStoreId(STORE_ID);
		product.setImageViolations(null);
		Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
		Mockito.when(
				productImageQcFeedbackService.findProductQcFeedbackResponseByProductCode(STORE_ID, product.getProductCode()))
				.thenReturn(null);
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails = productDistributionController
				.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
		Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
		Mockito.verify(productImageQcFeedbackService)
				.findProductQcFeedbackResponseByProductCode(STORE_ID, product.getProductCode());
		verify(this.productReviewerService).findProductReviewerByProductCode(
			Mockito.anyString());
		Assertions.assertEquals(5, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
						.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertFalse(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
				productDetails.getValue().getBrandApprovalStatus());
	}

	@Test
	public void getProductDetailsProductAndItemNotesTest() throws Exception {
		Product product = createProduct(true, true);
		product.setState(null);
		product.setStoreId(STORE_ID);
		product.setImageViolations(null);
		product.setProductNotes("{\"vendorNotes\" : [\"Incomplete or inappropriate content\"],\n"
				+ "\"ContentAdditionalNotes\" : \"notes\"}");
		ProductNotesResponse productNotesResponse =
				ProductNotesResponse.builder().vendorNotes(Arrays.asList("Incomplete or inappropriate content"))
						.contentAdditionalNotes("notes").build();
		product.getProductItems().get(0).setItemNotes("{\"vendorNotes\": [\"Blur or inappropriate images\"],\n"
				+ "\"vendorErrorFields\": [\"images\"]}");
		Mockito.when(mapper.readValue(anyString(), eq(ProductNotesResponse.class))).thenReturn(productNotesResponse);
		Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE)).thenReturn(product);
		Mockito.when(
				productImageQcFeedbackService.findProductQcFeedbackResponseByProductCode(STORE_ID, product.getProductCode()))
				.thenReturn(null);
		GdnRestSingleResponse<DistributionProductDetailResponse> productDetails = productDistributionController
				.getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10, PRODUCT_CODE);
		Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
		Mockito.verify(productImageQcFeedbackService)
				.findProductQcFeedbackResponseByProductCode(STORE_ID, product.getProductCode());
		verify(this.productReviewerService).findProductReviewerByProductCode(
			Mockito.anyString());
		Assertions.assertEquals(5, productDetails.getValue().getProductImages().size());
		Assertions.assertEquals(6,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().size());
		Assertions.assertEquals(4,
				productDetails.getValue().getProductItems().get(0).getProductItemImages().stream()
						.filter(DistributionProductImageResponse::getOriginalImage).count());
		Assertions.assertFalse(productDetails.getValue().isEnableImageFeedback());
		Assertions.assertEquals(BRAND_CODE, productDetails.getValue().getBrandCode());
		Assertions.assertEquals(BRAND_APPROVAL_STATUS,
				productDetails.getValue().getBrandApprovalStatus());
	}

  @Test public void getProductDetailsTestNotOk() throws Exception {
    productDistributionController
        .getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10,
            PRODUCT_CODE);
    Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(PRODUCT_CODE);
	verify(this.productReviewerService).findProductReviewerByProductCode(
		  Mockito.anyString());
  }

  @Test
   void getSummaryByMultipleFilterTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    productPage.getContent().get(0).setState(WorkflowState.PASSED);
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    List<WorkflowState> workflowStateList = new ArrayList<>();
    workflowStateList.add(WorkflowState.PASSED);
    workflowStateMap.put("id", workflowStateList);
    Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
        .thenReturn(workflowStateMap);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
    Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.any(), Mockito.any());
  }

  @Test
   void getSummaryByMultipleFilterTest_StateNull_CurrentVendorNull() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    productPage.getContent().get(0).setState(null);
    productPage.getContent().get(0).setCurrentVendor(null);
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    List<WorkflowState> workflowStateList = new ArrayList<>();
    workflowStateList.add(WorkflowState.PASSED);
    workflowStateMap.put("id", workflowStateList);
    Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
        .thenReturn(workflowStateMap);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
		Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.anyString(), Mockito.any());
  }

  @Test
   void getSummaryByMultipleFilterAscTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    distributionTaskMultipleFilterRequest.setTimeFilterType("");
    productPage.getContent().get(0).setState(WorkflowState.PASSED);
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    List<WorkflowState> workflowStateList = new ArrayList<>();
    workflowStateList.add(WorkflowState.PASSED);
    workflowStateMap.put("id", workflowStateList);
    Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
        .thenReturn(workflowStateMap);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("sortBy", ASC))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
		Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.any(), Mockito.any());
  }

  @Test
   void getSummaryByMultipleFilterDescTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    distributionTaskMultipleFilterRequest.setTimeFilterType("ALL");
    productPage.getContent().get(0).setState(WorkflowState.PASSED);
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
    List<WorkflowState> workflowStateList = new ArrayList<>();
    workflowStateList.add(WorkflowState.PASSED);
    workflowStateMap.put("id", workflowStateList);
    Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
        .thenReturn(workflowStateMap);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("sortBy", DESC))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
            (Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
		Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.any(), Mockito.any());
  }

	@Test
	public void getSummaryByMultipleFilterWithSortTest() throws Exception {
		DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
				new DistributionTaskMultipleFilterRequest();
		productPage.getContent().get(0).setState(WorkflowState.PASSED);
		String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
		Mockito.when(this.productService
				.getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
						(Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
		Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
		List<WorkflowState> workflowStateList = new ArrayList<>();
		workflowStateList.add(WorkflowState.PASSED);
		workflowStateMap.put("id", workflowStateList);
		Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
				.thenReturn(workflowStateMap);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
				.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
				.param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
				.param("requestId", REQUEST_ID).param("username", USERNAME).param("sort", ASC))
				.andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
		Mockito.verify(this.productService, Mockito.times(1))
				.getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
						(Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(this.productService, Mockito.times(1)).setVendorForProductList(Mockito.anyList());
		Mockito.verify(productConverterUtil).getProductListResponseWithoutProductId(Mockito.any(), Mockito.any());
	}

  @Test public void getSummaryByMultipleFilterEmptyProductTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    productPage = new PageImpl<Product>(new ArrayList<Product>());
		Mockito.when(this.productService.getAllProductDetailsWithMultipleFilter(
				(DistributionTaskMultipleFilterDTO) Mockito.any(), (Pageable) Mockito.any(),
				Mockito.anyString())).thenReturn(productPage);
    Assertions.assertTrue(productPage.getContent().isEmpty());
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
		Mockito.verify(this.productService, Mockito.times(1))
				.getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
						(Pageable) Mockito.any(), Mockito.anyString());
    Mockito.verify(productService, Mockito.times(0))
        .getWorkflowStatusForProducts(Mockito.anyList());
    Mockito.verifyNoMoreInteractions(productConverterUtil);
  }

  @Test public void getSummaryByMultipleFilterExceptionTest() throws Exception {
	    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
	        new DistributionTaskMultipleFilterRequest();
	    productPage.getContent().get(0).setState(WorkflowState.PASSED);
	    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
	    Mockito.when(this.productService
	        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
	            (Pageable) Mockito.any(), Mockito.anyString())).thenReturn(productPage);
	    Assertions.assertSame(WorkflowState.PASSED, productPage.getContent().get(0).getState());
	    Map<String, List<WorkflowState>> workflowStateMap = new HashMap<>();
	    List<WorkflowState> workflowStateList = new ArrayList<>();
	    workflowStateList.add(WorkflowState.PASSED);
	    workflowStateMap.put("id", workflowStateList);
	    Mockito.when(productService.getWorkflowStatusForProducts(Mockito.anyList()))
	        .thenReturn(workflowStateMap);
	    Mockito.when(this.productService
	            .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
	                (Pageable) Mockito.any(), Mockito.anyString())).thenThrow(Exception.class);
	    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
	        + ProductDistributionController.GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER)
	        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
	        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
	        .param("requestId", REQUEST_ID).param("username", USERNAME))
	        .andExpect(MockMvcResultMatchers.status().isOk())
	        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
	    Mockito.verify(this.productService, Mockito.times(1))
	        .getAllProductDetailsWithMultipleFilter((DistributionTaskMultipleFilterDTO) Mockito.any(),
	            (Pageable) Mockito.any(), Mockito.anyString());
	  }


  @Test public void countDistributionSummaryByMultipleFilterTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .countAllProductDetailsWithMultipleFilter(Mockito.anyBoolean(), Mockito.anyBoolean(), (DistributionTaskMultipleFilterDTO) Mockito.any())).thenReturn(new HashMap<String, Object>());
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("includeStatus", "true").param("includeVendors", "true")
        .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .countAllProductDetailsWithMultipleFilter(Mockito.anyBoolean(), Mockito.anyBoolean(), (DistributionTaskMultipleFilterDTO) Mockito.any());
  }

  @Test public void countDistributionSummaryByMultipleFilterNullRequestTest() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
        new DistributionTaskMultipleFilterRequest();
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskMultipleFilterRequest);
    Mockito.when(this.productService
        .countAllProductDetailsWithMultipleFilter(true, false, null)).thenReturn(new HashMap<String, Object>());
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("includeStatus", "true").param("includeVendors", "true")
        .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService, Mockito.times(1))
        .countAllProductDetailsWithMultipleFilter(Mockito.anyBoolean(), Mockito.anyBoolean(), (DistributionTaskMultipleFilterDTO) Mockito.any());
  }

  @Test public void countDistributionSummaryByFilterTestException() throws Exception {
    DistributionTaskMultipleFilterRequest distributionTaskFilterRequest =
        new DistributionTaskMultipleFilterRequest();
	  List statusList = new ArrayList<>();
	  statusList.add(WorkflowState.UNASSIGNED.name());
    distributionTaskFilterRequest.setStatusList(statusList);
    String request = OBJECT_MAPPER.writeValueAsString(distributionTaskFilterRequest);
    Mockito.when(this.productService
        .countAllProductDetailsWithMultipleFilter(Mockito.anyBoolean(), Mockito.anyBoolean(), (DistributionTaskMultipleFilterDTO) Mockito.any())).thenThrow(Exception.class);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
        + ProductDistributionController.COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("includeStatus", "true").param("includeVendors", "true")
        .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productService, Mockito.times(1))
        .countAllProductDetailsWithMultipleFilter(Mockito.anyBoolean(), Mockito.anyBoolean(), (DistributionTaskMultipleFilterDTO) Mockito.any());
  }

  @Test
   void isProductExistsTest() throws Exception {
    Mockito.when(productService.getProductByProductCodeAndMarkForDeleteFalse(Mockito.anyString())).thenReturn(product);
    GdnRestSimpleResponse<Boolean> response = productDistributionController
        .isProductExists(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE, false);
    Mockito.verify(productService).getProductByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(response.getValue());
  }

  @Test
   void isProductExists_WithExceptionTest() throws Exception {
		Mockito.doThrow(RuntimeException.class).when(productService)
				.getProductByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
		GdnRestSimpleResponse<Boolean> response = productDistributionController
        .isProductExists(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE, false);
    Mockito.verify(productService).getProductByProductCodeAndMarkForDeleteFalse(Mockito.anyString());
    Assertions.assertTrue(!response.isSuccess());
    Assertions.assertTrue(!response.getValue());
  }

	@Test
	public void isProductExistsAllProductsTest() throws Exception {
		Mockito.when(productService.getProductByCode(Mockito.anyString())).thenReturn(product);
		GdnRestSimpleResponse<Boolean> response = productDistributionController
				.isProductExists(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE, true);
		Mockito.verify(productService).getProductByCode(Mockito.anyString());
		Assertions.assertTrue(response.isSuccess());
		Assertions.assertTrue(response.getValue());
	}

	@Test
	public void isProductExistsAllProductsNullTest() throws Exception{
		GdnRestSimpleResponse<Boolean> response = productDistributionController
				.isProductExists(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE, true);
		Mockito.verify(productService).getProductByCode(Mockito.anyString());
		Assertions.assertTrue(response.isSuccess());
		Assertions.assertFalse(response.getValue());
	}

	@Test
	public void republishEditedProducts() throws Exception {
		Mockito.doNothing().when(productService).republishEditedProduct(Mockito.anyString());
		GdnBaseRestResponse response =
				productDistributionController
						.republishEditedProducts(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
		Mockito.verify(productService).republishEditedProduct(Mockito.anyString());
		Assertions.assertTrue(response.isSuccess());
	}

	@Test
	public void republishEditedProducts_WithExceptionTest() throws Exception{
		Mockito.doThrow(RuntimeException.class).when(productService).republishEditedProduct(Mockito.anyString());
		GdnBaseRestResponse response =
				productDistributionController
						.republishEditedProducts(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
		Mockito.verify(productService).republishEditedProduct(Mockito.anyString());
		Assertions.assertTrue(!response.isSuccess());
	}

  @Test public void getProductDetailsWithExceptionTestOk() throws Exception {
    Mockito.when(productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(Mockito.anyString()))
        .thenThrow(Exception.class);
    productDistributionController
        .getProductDetails(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, 0, 10,
            "product-code");
    Mockito.verify(productWrapperService).getAllProductDetailsByCodeAndMarkForDeleteFalse(Mockito.anyString());
  }

  @Test public void filterProductBusinessPartnerMapperByWorkFlowStateTestOk() throws Exception {
    Mockito.when(productService
        .findProductBusinessPartnerMapper(Mockito.any(WorkflowState.class), Mockito.anyString(),
            Mockito.any(Pageable.class), Mockito.anyBoolean(), Mockito.anyString()))
        .thenReturn(productBusinessPartnerMappers);
    GdnBaseRestResponse gdnBaseRestResponse = productDistributionController
        .filterProductBusinessPartnerMapperByWorkFlowState(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, 0, 25, false, "search criteria",
            WorkflowState.PASSED.toString());
    Mockito.verify(productService).findProductBusinessPartnerMapper(Mockito.any(WorkflowState.class), Mockito.anyString(),
            Mockito.any(Pageable.class), Mockito.anyBoolean(), Mockito.anyString());
  }

  @Test public void filterProductBusinessPartnerMapperByWorkFlowStateTestException() throws Exception {
	    Mockito.when(productService
	        .findProductBusinessPartnerMapper(Mockito.any(WorkflowState.class), Mockito.anyString(),
	            Mockito.any(Pageable.class), Mockito.anyBoolean(), Mockito.anyString()))
	        .thenReturn(null);
	    GdnBaseRestResponse gdnBaseRestResponse = productDistributionController
	        .filterProductBusinessPartnerMapperByWorkFlowState(STORE_ID, CHANNEL_ID, CLIENT_ID,
	            REQUEST_ID, USERNAME, 0, 25, false, "search criteria",
	            WorkflowState.PASSED.toString());
	    Mockito.verify(productService).findProductBusinessPartnerMapper(Mockito.any(WorkflowState.class), Mockito.anyString(),
	            Mockito.any(Pageable.class), Mockito.anyBoolean(), Mockito.anyString());
  }

  @Test public void countProductStatusForVendorTestOk() throws Exception {
    GdnRestSingleResponse<VendorProductStatusResponse> response = productDistributionController
	        .countProductStatusForVendor(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
	            "product-code");
	    Mockito.verify(vendorService).findByVendorCode(Mockito.anyString());
	    Mockito.verify(productService).findProductStatusForVendor((Vendor) Mockito.any(), Mockito.anyString());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertEquals(response.getValue().getInReview().intValue(), IN_REVIEW_COUNT);
  }

  @Test public void countProductStatusForVendorNull() throws Exception {
		Mockito.when(vendorService.findByVendorCode(Mockito.anyString()))
			.thenReturn(null);
	    productDistributionController
	        .countProductStatusForVendor(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
	            "product-code");
	    Mockito.verify(vendorService).findByVendorCode(Mockito.anyString());
  }

  @Test public void getProductDetailsForAllProductTypesTest() throws Exception {
		Product product = createProduct(true, true);
		product.setImageViolations(null);
		Mockito.when(productService.getDetailsForAnyProductTypeByCode(PRODUCT_CODE)).thenReturn(product);
		Mockito.when(productImageQcFeedbackService
				.findProductQcFeedbackResponseByProductCode(product.getStoreId(), product.getProductCode()))
				.thenReturn(new ProductImageQcFeedbackResponse());
		productDistributionController
				.getProductDetailsForAllProductTypes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
		Mockito.verify(productService).getDetailsForAnyProductTypeByCode(PRODUCT_CODE);
		Mockito.verify(productImageQcFeedbackService)
				.findProductQcFeedbackResponseByProductCode(product.getStoreId(), product.getProductCode());
	}

  @Test public void getProductDetailsForAllProductTypesExceptionTest() throws Exception {
	    GdnRestSingleResponse<DistributionProductDetailResponse> response = productDistributionController
	        .getProductDetailsForAllProductTypes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
	            null);
	    Assertions.assertFalse(response.isSuccess());
	    Assertions.assertTrue(
					response.getErrorMessage().contains(ExceptionMsg.EXCEPTION_PRODUCT_CODE_NULL.getValue()));
	    Assertions.assertNull(response.getErrorCode());

  }

	@Test
	public void getProductDomainEventModelResponseTest() throws Exception {
		Product product = createProduct(true, true);
		product.setState(WorkflowState.PASSED);
		product.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
  	Mockito.when(
				productService.getDetailsForAnyProductTypeByCode(Mockito.anyString()))
				.thenReturn(product);
		Mockito.when(productPublisherService
				.convertProductToProductDomainEventModel((Product) Mockito.any(), Mockito.eq(false)))
				.thenReturn(new PDTProductDomainEventModel());
		productDistributionController.getProductDomainEventModelResponse(
				STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
				PRODUCT_CODE);
		Mockito.verify(productService).getDetailsForAnyProductTypeByCode(
				Mockito.anyString());
		Mockito.verify(productPublisherService).convertProductToProductDomainEventModel(
				(Product) Mockito.any(), Mockito.eq(false));
	}

	@Test
	public void getProductDomainEventModelResponseAutoHealEnabledTest() throws Exception {
		ReflectionTestUtils.setField(productDistributionController, "autoHealEnabledForGetProductDomainModelResponseApi", true);
		Product product = createProduct(true, true);
		product.setState(WorkflowState.PASSED);
		product.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
		Mockito.when(
						productService.getDetailsForAnyProductTypeByCode(Mockito.anyString()))
				.thenReturn(product);
		Mockito.when(productPublisherService
						.convertProductToProductDomainEventModel((Product) Mockito.any(), Mockito.eq(false)))
				.thenReturn(new PDTProductDomainEventModel());
		Mockito.when(productService.autoHealProductData(Mockito.any(Product.class),
				Mockito.eq(Constants.AUTOHEAL_GET_PRODUCT_MODEL_API))).thenReturn(product);
		productDistributionController.getProductDomainEventModelResponse(
				STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
				PRODUCT_CODE);
		Mockito.verify(productService).getDetailsForAnyProductTypeByCode(
				Mockito.anyString());
		Mockito.verify(productPublisherService).convertProductToProductDomainEventModel(
				(Product) Mockito.any(), Mockito.eq(false));
		Mockito.verify(productService).autoHealProductData(Mockito.any(Product.class), Mockito.eq(Constants.AUTOHEAL_GET_PRODUCT_MODEL_API));
	}

	@Test
	public void getProductDomainEventModelResponseExceptionTest()
			throws Exception {
		GdnRestSingleResponse response = productDistributionController.getProductDomainEventModelResponse(
				STORE_ID, CHANNEL_ID, CLIENT_ID, null, USERNAME, PRODUCT_CODE);
        Assertions.assertFalse(response.isSuccess());
        Assertions.assertTrue(
						response.getErrorMessage().contains(ErrorCategory.REQUIRED_PARAMETER.getMessage()));
        Assertions.assertNull(response.getErrorCode());
	}

  @Test public void getBusinesspartnerForVendorWithSuccess() throws Exception {
    Mockito.when(productService.getvendorIdByVendorCode(VENDOR_CODE)).thenReturn(VENDOR_ID);
    Assertions.assertTrue(!StringUtils.isEmpty(VENDOR_ID));
    Mockito.when(productService
        .getBusinessPartnerForVendor(Mockito.anyString(), Mockito.any(Pageable.class)))
        .thenReturn(productBusinessPartnerMappers);
    this.mockMvc.perform(MockMvcRequestBuilders.get(ProductDistributionController.BASE_PATH
        + ProductDistributionController.GET_BUSINESS_PARTNER_LIST_FOR_VENDOR)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("vendorCode", VENDOR_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
  }

  @Test public void getBusinesspartnerForVendorWithFailure() throws Exception {
    Mockito.when(productService.getvendorIdByVendorCode(VENDOR_CODE)).thenReturn("");
  }

  @Test
	public void getProductCodeListTest_success() throws Exception{
		List<String> productList = Arrays.asList("test1", "test2");
		ProductCodeListRequest request = new ProductCodeListRequest(productList);
		Mockito.when(productService.getProductCodeList(productList)).thenReturn(productList);
		GdnRestSingleResponse<ProductListResponse> result = productDistributionController
				.getProductCodeList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, request);
		Mockito.verify(productService).getProductCodeList(productList);
		Assertions.assertTrue(result.isSuccess());
		Assertions.assertEquals(productList, result.getValue().getProductList());
	}

	@Test
	public void getProductCodeListTest_fail() throws Exception{
		List<String> productList = Arrays.asList("test1", "test2");
		ProductCodeListRequest request = new ProductCodeListRequest(productList);
		Mockito.when(productService.getProductCodeList(productList)).thenThrow(new Exception());
		GdnRestSingleResponse<ProductListResponse> result = productDistributionController
				.getProductCodeList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, request);
		Mockito.verify(productService).getProductCodeList(productList);
		Assertions.assertFalse(result.isSuccess());
	}

	@Test
	public void rejectProductTest() throws Exception {
		RejectProductListRequest rejectProductListRequest = new RejectProductListRequest();
		rejectProductListRequest.setProducts(new ArrayList<>());
		RejectProductVendorRequest request = new RejectProductVendorRequest();
		rejectProductListRequest.getProducts().add(request);
		this.productDistributionController
				.rejectProduct(ProductDistributionControllerTest.STORE_ID, ProductDistributionControllerTest.CHANNEL_ID,
						ProductDistributionControllerTest.CLIENT_ID, ProductDistributionControllerTest.REQUEST_ID,
						ProductDistributionControllerTest.USERNAME, rejectProductListRequest);
		Mockito.verify(this.productService).rejectProduct(Mockito.any());
	}

	@Test
	public void rejectProductTestException() throws Exception {
		this.productDistributionController
				.rejectProduct(ProductDistributionControllerTest.STORE_ID, ProductDistributionControllerTest.CHANNEL_ID,
						ProductDistributionControllerTest.CLIENT_ID, ProductDistributionControllerTest.REQUEST_ID,
						ProductDistributionControllerTest.USERNAME, null);
	}

  @Test
   void getProductInReviewStatusTest() throws Exception {
	  Mockito.when(
			  this.productService.getProductStatusByVendor(STORE_ID, VENDOR_CODE, null, null, null))
		  .thenReturn(productInReviewCount);
	  String request = OBJECT_MAPPER.writeValueAsString(primaryFilterRequest);
	  this.mockMvc.perform(MockMvcRequestBuilders.get(ProductDistributionController.BASE_PATH
			  + ProductDistributionControllerTest.GET_PRODUCT_STATUS_IN_REVIEW)
		  .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
		  .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
		  .param("requestId", REQUEST_ID).param("username", USERNAME)
		  .param("vendorCode", VENDOR_CODE)).andExpect(MockMvcResultMatchers.status().isOk());
	  Mockito.verify(this.productService)
		  .getProductStatusByVendor(STORE_ID, VENDOR_CODE, null, null, null);
  }

	@Test
	public void getProductInReviewStatusPostLiveTrueTest() throws Exception {
		Mockito.when(this.productService.getProductStatusByVendor(STORE_ID, VENDOR_CODE,
				Boolean.TRUE, null,
				null))
				.thenReturn(productInReviewCount);
		this.mockMvc.perform(MockMvcRequestBuilders.get(ProductDistributionController.BASE_PATH
				+ ProductDistributionControllerTest.GET_PRODUCT_STATUS_IN_REVIEW)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME).param("vendorCode", VENDOR_CODE)
        .param("postLive", String.valueOf(Boolean.TRUE))).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService).getProductStatusByVendor(STORE_ID, VENDOR_CODE,
			Boolean.TRUE, null,
			null);
	}

	@Test
	public void getProductReviewConfigCountsTest() throws Exception {
		Mockito.when(this.productService.getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE))
				.thenReturn(productReviewConfigCount);
		this.mockMvc.perform(MockMvcRequestBuilders.get(ProductDistributionController.BASE_PATH
				+ ProductDistributionControllerTest.GET_PRODUCT_REVIEW_CONFIG_COUNTS)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME).param("vendorCode", VENDOR_CODE)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService).getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE);
	}

	@Test
	public void getProductReviewConfigCountsTest_exceptionTest() throws Exception {
		Mockito.when(this.productService.getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE))
				.thenThrow(Exception.class);
		this.mockMvc.perform(MockMvcRequestBuilders.get(ProductDistributionController.BASE_PATH
				+ ProductDistributionControllerTest.GET_PRODUCT_REVIEW_CONFIG_COUNTS)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
				.param("requestId", REQUEST_ID).param("username", USERNAME)
				.param("vendorCode", VENDOR_CODE)).andExpect(MockMvcResultMatchers.status().isOk())
				.andExpect(jsonPath("$.success", Matchers.equalTo(false)));
		Mockito.verify(this.productService)
				.getReviewConfigProductCountByVendor(STORE_ID, VENDOR_CODE);
	}

	@Test
	public void getBusinessPartnerListTest() throws Exception {
		Mockito.when(this.productService.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE, SIZE))
				.thenReturn(productBusinessPartnerMapperResponseGdnRestListResponse);
		String request = OBJECT_MAPPER.writeValueAsString(primaryFilterRequest);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.FILTER_BUSINESS_PARTNERS_BY_PRODUCT_LIST)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService).getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE, SIZE);

	}

	@Test
	public void getBusinessPartnerList_expectException() throws Exception {
		Mockito.when(this.productService.getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE, SIZE))
				.thenThrow(RuntimeException.class);
		String request = OBJECT_MAPPER.writeValueAsString(primaryFilterRequest);
		try {
			this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
					+ ProductDistributionController.FILTER_BUSINESS_PARTNERS_BY_PRODUCT_LIST)
					.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
					.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
					.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		} catch (Exception e) {
		} finally {
			Mockito.verify(this.productService).getBusinessPartnerList(STORE_ID, REQUEST_ID, primaryFilterDTO, PAGE, SIZE);
		}
	}

	@Test
	public void getAssigneeListTest() throws Exception {
		Mockito.when(this.productService.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO))
				.thenReturn(assigneeResponseList);
		String request = OBJECT_MAPPER.writeValueAsString(primaryFilterRequest);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.FILTER_ASSIGNEES_BY_PRODUCT_LIST)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService).getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
	}

	@Test
	public void getAssigneeList_expectException() throws Exception {
		Mockito.when(this.productService.getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO))
				.thenThrow(RuntimeException.class);
		String request = OBJECT_MAPPER.writeValueAsString(primaryFilterRequest);
		try {
			this.mockMvc.perform(MockMvcRequestBuilders.post(
					ProductDistributionController.BASE_PATH + ProductDistributionController.FILTER_ASSIGNEES_BY_PRODUCT_LIST)
					.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
					.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
					.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		} catch (Exception e) {

		} finally {
			Mockito.verify(this.productService).getAssigneeList(STORE_ID, REQUEST_ID, primaryFilterDTO);
		}
	}

	@Test
	public void getProductListTest() throws Exception {
		Mockito.when(this.productService.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE))
				.thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, SIZE), SIZE));
		String request = OBJECT_MAPPER.writeValueAsString(summaryFilterDTO);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.FILTER_SUMMARY)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService).getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE);
	}

	@Test
	public void getProductList_expectException() throws Exception {
		Mockito.when(this.productService.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE))
				.thenThrow(Exception.class);
		String request = OBJECT_MAPPER.writeValueAsString(summaryFilterDTO);
		try {
			this.mockMvc.perform(MockMvcRequestBuilders
					.post(ProductDistributionController.BASE_PATH + ProductDistributionController.FILTER_SUMMARY)
					.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
					.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
					.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		} catch (Exception e) {
			Mockito.verify(this.productService).getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE);
		}
	}

	@Test
	public void sendProductBackToVendorTest() throws Exception {
		this.mockMvc.perform(MockMvcRequestBuilders
				.get(ProductDistributionController.BASE_PATH + ProductDistributionController.SEND_PRODUCT_BACK_TO_VENDOR)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME).param("productCode", PRODUCT_CODE))
				.andExpect(MockMvcResultMatchers.status().isOk())
				.andExpect(jsonPath("$.success", Matchers.equalTo(true)));
		Mockito.verify(this.productWrapperService).sendProductBackToVendorAndReindexSolr(PRODUCT_CODE);
	}

  @Test
   void detectEditByMerchantTest() throws Exception {
    Mockito.when(productService.getEditedByMerchant(PRODUCT_CODE, VERSION)).thenReturn(false);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(
        ProductDistributionController.BASE_PATH
            + ProductDistributionController.DETECT_EDIT_BY_MERCHANT.replaceAll("\\{productCode\\}", PRODUCT_CODE))
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("requestId", REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .param("version", String.valueOf(VERSION));
    this.mockMvc.perform(request)
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.equalTo(false)));
    Mockito.verify(this.productService).getEditedByMerchant(PRODUCT_CODE, VERSION);
  }

  @Test
   void detectEditByMerchantExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class)
        .when(productService)
        .getEditedByMerchant(PRODUCT_CODE, VERSION);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.get(
        ProductDistributionController.BASE_PATH
            + ProductDistributionController.DETECT_EDIT_BY_MERCHANT.replaceAll("\\{productCode\\}", PRODUCT_CODE))
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("requestId", REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .param("version", String.valueOf(VERSION));
    this.mockMvc.perform(request)
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.equalTo(true)));
    Mockito.verify(this.productService).getEditedByMerchant(PRODUCT_CODE, VERSION);
  }

  @Test
   void sendProductBackToVendorFaultyImageTest() throws Exception {
    FilterSummaryRequest filterSummaryRequest = new FilterSummaryRequest();
    filterSummaryRequest.setFaultyImageType(BLUR_PREDICTION);
    summaryFilterDTO.setFaultyImageType(BLUR_PREDICTION);
    Mockito.when(this.productService.getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, SIZE), SIZE));

    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(ProductDistributionController.BASE_PATH + ProductDistributionController.FILTER_SUMMARY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(filterSummaryRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));

    Mockito.verify(this.productService).getProductList(STORE_ID, REQUEST_ID, USERNAME, summaryFilterDTO, PAGE, SIZE);
  }

	@Test
	public void sendProductBackToVendorExceptionTest() throws Exception {
  	Mockito.doThrow(Exception.class).when(productWrapperService).sendProductBackToVendorAndReindexSolr(PRODUCT_CODE);
		this.mockMvc.perform(MockMvcRequestBuilders
				.get(ProductDistributionController.BASE_PATH + ProductDistributionController.SEND_PRODUCT_BACK_TO_VENDOR)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME).param("productCode", PRODUCT_CODE))
				.andExpect(MockMvcResultMatchers.status().isOk())
				.andExpect(jsonPath("$.success", Matchers.equalTo(false)));
		Mockito.verify(this.productWrapperService).sendProductBackToVendorAndReindexSolr(PRODUCT_CODE);
	}

	@Test
	public void getReviewConfigCountTest() throws Exception {
		Mockito.when(this.productService.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE))
				.thenReturn(productReviewCountByConfig);
		this.mockMvc.perform(MockMvcRequestBuilders
				.get(ProductDistributionController.BASE_PATH + ProductDistributionController.GET_REVIEW_CONFIG_COUNT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
				.param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
				.param("vendorCode", VENDOR_CODE).param("postLive", String.valueOf(Boolean.FALSE)))
				.andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService)
				.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
	}

	@Test
	public void getReviewConfigCount_exceptionTest() throws Exception {
		Mockito.when(this.productService.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE))
				.thenThrow(Exception.class);
		this.mockMvc.perform(MockMvcRequestBuilders
				.get(ProductDistributionController.BASE_PATH + ProductDistributionController.GET_REVIEW_CONFIG_COUNT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
				.param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
				.param("vendorCode", VENDOR_CODE).param("postLive", String.valueOf(Boolean.FALSE)))
				.andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)));
		Mockito.verify(this.productService)
				.getReviewConfigProductCountByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
	}

	@Test
	public void sendProductToAutoNeedRevisionTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(autoNeedRevisionRequest);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.SEND_PRODUCT_TO_AUTO_NEED_REVISION)
				.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
				.content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
				.param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productWrapperService)
				.updateProductToAutoNeedRevision(eq(STORE_ID), any(AutoNeedRevisionRequest.class), anyBoolean());
	}

	@Test
	public void sendProductToAutoNeedRevisionExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.productWrapperService)
        .updateProductToAutoNeedRevision(eq(STORE_ID), any(AutoNeedRevisionRequest.class), anyBoolean());
		String request = OBJECT_MAPPER.writeValueAsString(autoNeedRevisionRequest);
		this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
				+ ProductDistributionController.SEND_PRODUCT_TO_AUTO_NEED_REVISION)
				.accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
				.content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
				.param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWrapperService)
        .updateProductToAutoNeedRevision(eq(STORE_ID), any(AutoNeedRevisionRequest.class), anyBoolean());

	}

	@Test
	public void productRetryStatusUpdateTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(productRetryStatusUpdate);
		this.mockMvc.perform(MockMvcRequestBuilders.post(
					ProductDistributionController.BASE_PATH + ProductDistributionController.PRODUCT_RETRY_STATUS_UPDATE,
					PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
				.content(request).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
				.param("requestId", REQUEST_ID).param("username", USERNAME))
			.andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService)
			.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
	}

	@Test
	public void productRetryStatusUpdate_expectException() throws Exception {
		Mockito.doThrow(Exception.class).when(this.productService)
			.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
		String request = OBJECT_MAPPER.writeValueAsString(productRetryStatusUpdate);
		try {
			this.mockMvc.perform(MockMvcRequestBuilders.post(ProductDistributionController.BASE_PATH
						+ ProductDistributionController.PRODUCT_RETRY_STATUS_UPDATE, PRODUCT_CODE)
					.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
					.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
					.param("requestId", REQUEST_ID).param("username", USERNAME))
				.andExpect(MockMvcResultMatchers.status().isOk());
		} catch (Exception e) {
			Mockito.verify(this.productService)
				.updateProductRetryStatus(STORE_ID, PRODUCT_CODE, productRetryStatusUpdate);
		}
	}

	@Test
	public void fetchProductsForAutoAssignmentTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(boostedProductFilterRequest);
		Mockito.when(productService.filterProductWithBoostForAutoAssignment(STORE_ID, REQUEST_ID, USERNAME,
						boostedProductFilterRequest, PAGE, NEW_SIZE))
				.thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(PAGE, NEW_SIZE), NEW_SIZE));
		this.mockMvc.perform(MockMvcRequestBuilders.post(
						ProductDistributionController.BASE_PATH + ProductDistributionController.FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService)
				.filterProductWithBoostForAutoAssignment(STORE_ID, REQUEST_ID, USERNAME, boostedProductFilterRequest, PAGE,
						NEW_SIZE);
	}

	@Test
	public void fetchProductsForAutoAssignmentExceptionTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(boostedProductFilterRequest);
		Mockito.doThrow(RuntimeException.class).when(this.productService)
				.filterProductWithBoostForAutoAssignment(STORE_ID, REQUEST_ID, USERNAME, boostedProductFilterRequest, PAGE,
						NEW_SIZE);
		this.mockMvc.perform(MockMvcRequestBuilders.post(
						ProductDistributionController.BASE_PATH + ProductDistributionController.FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(this.productService)
				.filterProductWithBoostForAutoAssignment(STORE_ID, REQUEST_ID, USERNAME, boostedProductFilterRequest, PAGE,
						NEW_SIZE);
	}

	@Test
	public void updateProductBrand() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(changeBrandRequest);
		GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
		Mockito.doNothing().when(productWrapperService).updateBrandInProductAndProductItems(changeBrandRequest);
		this.mockMvc.perform(MockMvcRequestBuilders
				.post(ProductDistributionController.BASE_PATH + ProductDistributionController.UPDATE_PRODUCT_BRAND)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(productWrapperService).updateBrandInProductAndProductItems(changeBrandRequest);
	}

	@Test
	public void updateProductBrandExceptionTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(changeBrandRequest);
		GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
		Mockito.doThrow(new ApplicationRuntimeException()).when(productWrapperService).updateBrandInProductAndProductItems(changeBrandRequest);
		this.mockMvc.perform(MockMvcRequestBuilders
				.post(ProductDistributionController.BASE_PATH + ProductDistributionController.UPDATE_PRODUCT_BRAND)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(productWrapperService).updateBrandInProductAndProductItems(changeBrandRequest);
	}

	@Test
	public void updateProductAppeal() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(appealProductRequest);
		Mockito.when(productService.updateAppealProduct(appealProductRequest, STORE_ID))
			.thenReturn(AppealProductResponse.builder().build());
		this.mockMvc.perform(MockMvcRequestBuilders.post(
					ProductDistributionController.BASE_PATH + ProductDistributionController.APPEAL_PRODUCT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
				.param("requestId", REQUEST_ID).param("username", USERNAME))
			.andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(productService).updateAppealProduct(appealProductRequest, STORE_ID);
	}

	@Test
	public void updateProductAppealExceptionTest() throws Exception {
		String request = OBJECT_MAPPER.writeValueAsString(appealProductRequest);
		Mockito.doThrow(new ApplicationRuntimeException()).when(productService)
				.updateAppealProduct(appealProductRequest, STORE_ID);
		this.mockMvc.perform(MockMvcRequestBuilders.post(
						ProductDistributionController.BASE_PATH + ProductDistributionController.APPEAL_PRODUCT)
				.contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).content(request)
				.param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
				.param("username", USERNAME)).andExpect(MockMvcResultMatchers.status().isOk());
		Mockito.verify(productService).updateAppealProduct(appealProductRequest, STORE_ID);
	}
}
