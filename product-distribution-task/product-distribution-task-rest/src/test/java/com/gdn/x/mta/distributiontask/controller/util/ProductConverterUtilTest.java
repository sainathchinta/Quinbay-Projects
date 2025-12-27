package com.gdn.x.mta.distributiontask.controller.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.type.ProductLabels;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.ProductListRequestDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

/**
 * Created by virajjasani on 12/10/16.
 */
public class ProductConverterUtilTest {

  private static final String CATEGORY_CODE = "category-code";
  private static final String STOREID = "10001";
  private static final String ATTRIBUTE_CODE = "attr-code";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "desc";
  private static final String DATE = "2016-12-12 00:00";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final long TOTAL = 1;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
  private static final String DEFAULT_CONTENT_ASSIGNEE = "content assignee";
  private static final String CREATED_BY = "Migrated Created By";
  private static final String BRAND = "Brand";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ID = "31hj134-jbeeqw8-jkdsa2-ksdq234";
  private static final String VENDOR_CODE = "vendor_code";
  private static final String BLUR = "Blur";
  private static final String GOOGLE_RESTRICTED = "Google restriction";
  private static final String ORDERED_VIOLATION = "Pornography:Pornography,Illegal drugs:Illegal drugs,Brand mismatch:Brand,Google restriction:Google restriction,Cigarette:Cigarette,Prescription drugs:Prescription drugs,Prohibited drugs:Prohibited drugs,Other e-commerce/social media logos:Other e-commerce/social media logos,Watermark:Watermark,Blur:Blur,Text:Text";

  private final Vendor currentVendor = new Vendor();
  private ProductDetailResponse productDetailResponse;
  private final Product product = new Product();
  private final List<Product> productList = new ArrayList<>();
  private Page<Product> productPage;

  @InjectMocks
  private ProductConverterUtil productConverterUtil;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productDetailResponse = new ProductDetailResponse();
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    productCategoryResponse.setCategory(categoryResponse);
    productDetailResponse.setProductCategoryResponses(List.of(productCategoryResponse));
    productDetailResponse.setImages(List.of(new Image()));
    ProductAttributeValueResponse productAttributeValueResponse =
        new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    ProductAttributeValueResponse productAttributeValueResponseAllowed =
        new ProductAttributeValueResponse();
    productAttributeValueResponseAllowed
        .setAllowedAttributeValue(new AllowedAttributeValueResponse());
    ProductAttributeValueResponse productAttributeValueResponsePredefined =
        new ProductAttributeValueResponse();
    productAttributeValueResponsePredefined
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueResponse());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse
        .setProductAttributeValues(List.of(productAttributeValueResponse));
    ProductAttributeResponse productAttributeResponseAllowed = new ProductAttributeResponse();
    productAttributeResponseAllowed.setAttribute(attributeResponse);
    productAttributeResponseAllowed
        .setProductAttributeValues(List.of(productAttributeValueResponseAllowed));
    ProductAttributeResponse productAttributeResponsePredefined = new ProductAttributeResponse();
    productAttributeResponsePredefined.setAttribute(attributeResponse);
    productAttributeResponsePredefined
        .setProductAttributeValues(List.of(productAttributeValueResponsePredefined));
    productDetailResponse.setProductAttributeResponses(Arrays
        .asList(productAttributeResponse, productAttributeResponseAllowed,
            productAttributeResponsePredefined));
    Image itemImage = new Image();
    itemImage.setOriginalImage(true);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(List.of(itemImage));
    productDetailResponse.setProductItemResponses(
        new HashSet<ProductItemResponse>(List.of(productItemResponse)));
    product.setCreatedBy(CREATED_BY);

    product.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    product.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    currentVendor.setAbleToReject(Boolean.TRUE);
    currentVendor.setQcRequired(Boolean.TRUE);
    product.setCurrentVendor(currentVendor);
    productList.add(product);
    productPage = new PageImpl<>(productList, PageRequest.of(PAGE, SIZE), TOTAL);

    ReflectionTestUtils.setField(productConverterUtil, "vendorProductLabelsOrdered", ORDERED_VIOLATION);
  }

  @Test
   void convertProductDetailResponseToProduct() throws Exception {
    Product product =
        productConverterUtil.convertProductDetailResponseToProduct(productDetailResponse, true);
    Assertions.assertEquals(CATEGORY_CODE, product.getCategoryCode());
    Assertions.assertEquals(1, product.getProductImages().size());
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(3, product.getProductAttributes().size());
  }
  
  @Test
   void convertProductDetailResponseToProduct2() throws Exception {
    Product product =
        productConverterUtil.convertProductDetailResponseToProduct(productDetailResponse, false);
    Assertions.assertEquals(CATEGORY_CODE, product.getCategoryCode());
    Assertions.assertEquals(1, product.getProductImages().size());
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(3, product.getProductAttributes().size());
  }

  @Test
   void convertProductDetailResponseToProduct3() throws Exception {
    productDetailResponse.getImages().get(0).setOriginalImage(Boolean.TRUE);
    Product product =
        productConverterUtil.convertProductDetailResponseToProduct(productDetailResponse, false);
    Assertions.assertTrue(product.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(
        product.getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    Assertions.assertEquals(CATEGORY_CODE, product.getCategoryCode());
    Assertions.assertEquals(1, product.getProductImages().size());
    Assertions.assertEquals(1, product.getProductItems().size());
    Assertions.assertEquals(3, product.getProductAttributes().size());
  }
  
  @Test
   void getProductListRequestServiceDTOTest() throws Exception {
	ProductListRequest productListRequest = new ProductListRequest();
	List<WorkflowWebState> workflowWebState = new ArrayList<WorkflowWebState>();
	workflowWebState.add(WorkflowWebState.PASSED);
	productListRequest.setWorkflowWebState(workflowWebState);
	productListRequest.setStartDate(DATE);
	productListRequest.setEndDate(DATE);
	ProductListRequestDTO response = productConverterUtil.getProductListRequestServiceDTO(STOREID, productListRequest);
	Assertions.assertEquals(productListRequest.getWorkflowWebState().size(),
      response.getWorkflowState().size());
  }
  
  @Test
   void getProductListRequestServiceDTONullDataTest() throws Exception {
	ProductListRequest productListRequest = new ProductListRequest();
	ProductListRequestDTO response = productConverterUtil.getProductListRequestServiceDTO(STOREID, productListRequest);
	Assertions.assertNull(response.getWorkflowState());
  }

  @Test
   void convertProductsToDistributionProductResponseTest() {
    List<DistributionProductResponse> response =
        this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertFalse(response.get(0).isProductApproved());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
  }

  @Test
   void convertProductsToDistributionProductResponseTest_nullCurrentVendor() {
    productPage.getContent().get(0).setCurrentVendor(null);
    List<DistributionProductResponse> response =
        this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertNull(response.get(0).getCurrentVendor());
  }

  @Test
   void convertProductsToDistributionProductResponseTest_nullState() {
    productPage.getContent().get(0).setState(null);
    List<DistributionProductResponse> response =
        this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertNull(response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertFalse(response.get(0).isProductApproved());
  }

  @Test
   void getFilterProductSummaryTest() {
    Page<Product> productList = new PageImpl<Product>(createProductList());
    GdnRestListResponse<DistributionProductResponse> filterProductSummary =
        productConverterUtil.getFilterProductSummary(PageRequest.of(PAGE, SIZE), REQUEST_ID, productList);
    Assertions.assertEquals(PRODUCT_CODE,
        filterProductSummary.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        filterProductSummary.getContent().get(0).getProductName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        filterProductSummary.getContent().get(0).getVendorName());
  }

  @Test
   void getFilterProductSummaryTest_nullState() {
    Page<Product> productList = new PageImpl<Product>(createProductList());
    productList.getContent().get(0).setState(null);
    GdnRestListResponse<DistributionProductResponse> filterProductSummary =
        productConverterUtil.getFilterProductSummary(PageRequest.of(PAGE, SIZE), REQUEST_ID, productList);
    Assertions.assertEquals(PRODUCT_CODE,
        filterProductSummary.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        filterProductSummary.getContent().get(0).getProductName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        filterProductSummary.getContent().get(0).getVendorName());
  }

  @Test
   void getFilterProductSummaryWithNullVendorTest() {
    Page<Product> productList = new PageImpl<Product>(createProductList());
    productList.getContent().get(0).setCurrentVendor(null);
    GdnRestListResponse<DistributionProductResponse> filterProductSummary =
        productConverterUtil.getFilterProductSummary(PageRequest.of(PAGE, SIZE), REQUEST_ID, productList);
    Assertions.assertEquals(PRODUCT_CODE,
        filterProductSummary.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        filterProductSummary.getContent().get(0).getProductName());
    Assertions.assertNull(filterProductSummary.getContent().get(0).getVendorName());
  }

  private List<Product> createProductList() {
    Product product = new Product.Builder().productName(PRODUCT_NAME).productCode(PRODUCT_CODE)
        .productAttributes(new ArrayList<>())
        .productImages(new ArrayList<>()).productItems(new ArrayList<>())
        .build();
    Vendor currentVendor = new Vendor();
    currentVendor.setName(DEFAULT_BUSINESS_PARTNER_NAME);
    currentVendor.setStoreId(STOREID);
    currentVendor.setUpdatedDate(new Date());
    currentVendor.setMarkForDelete(false);
    currentVendor.setAbleToReject(false);
    currentVendor.setQcRequired(false);
    currentVendor.setSlaInDays(10);
    currentVendor.setId(ID);
    product.setCurrentVendor(currentVendor);
    product.setState(WorkflowState.PASSED);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    return productList;
  }

  @Test
   void getProductListResponseWithoutProductIdTest() throws Exception{
    Product product = createProductList().get(0);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertTrue(distributionProductResponse.isProductApproved());
  }

  @Test
   void getProductListResponseWithoutProductIdTest_Null()throws Exception{
    Product product = createProductList().get(0);
    product.setState(null);
    product.setCurrentVendor(null);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertFalse(distributionProductResponse.isProductApproved());
  }

  @Test
   void getProductListResponseWithoutProductIdTest_Content_And_Image_Approved() throws Exception{
    Product product = createProductList().get(0);
    product.setState(WorkflowState.PASSED);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertTrue(distributionProductResponse.isProductApproved());
  }

  @Test
   void getProductListResponseWithoutProductIdTest_Content_Approved() throws Exception{
    Product product = createProductList().get(0);
    product.setState(WorkflowState.PASSED);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertTrue(distributionProductResponse.isProductApproved());
  }

  @Test
   void getProductListResponseWithoutProductIdTest_Image_Approved() throws Exception{
    Product product = createProductList().get(0);
    product.setState(WorkflowState.PASSED);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertTrue(distributionProductResponse.isProductApproved());
  }

  @Test
   void getProductListResponseWithoutProductIdTest_Image_Approved_NotPassed() throws Exception{
    Product product = createProductList().get(0);
    product.setState(WorkflowState.UNASSIGNED);
    DistributionProductResponse distributionProductResponse = productConverterUtil.getProductListResponseWithoutProductId(VENDOR_CODE, product);
    Assertions.assertFalse(distributionProductResponse.isProductApproved());
  }

  @Test
   void convertProductsToDistributionProductResponse_approvedProductTest() {
    productPage.getContent().get(0).setState(WorkflowState.PASSED);
    productPage.getContent().get(0).setReviewType(ReviewType.CONTENT);
    List<DistributionProductResponse> response =
      this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.PASSED, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
  }

  @Test
   void convertToDistributionProductResponseTest() {
    ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO = new ProductAndReviewerDetailsDTO();
    product.setPredictedBrand(BRAND);
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    productAndReviewerDetailsDTO.setProduct(product);
    productAndReviewerDetailsDTO.setProductReviewer(new ProductReviewer());
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOPage =
        new PageImpl<>(List.of(productAndReviewerDetailsDTO));
    List<DistributionProductResponse> responses =
        productConverterUtil.convertToDistributionProductResponse(productAndReviewerDetailsDTOPage, ORDERED_VIOLATION);
    Assertions.assertEquals(product.getProductCode(), responses.get(0).getProductCode());
    Assertions.assertEquals(BRAND, responses.get(0).getPredictedBrand());
    Assertions.assertTrue(responses.get(0).isB2cActivated());
    Assertions.assertTrue(responses.get(0).isB2bActivated());
  }

  @Test
   void convertProductsToDistributionProductResponse_approvedProductByDateTest() {
    productPage.getContent().get(0).setReviewType(ReviewType.CONTENT);
    List<DistributionProductResponse> response =
      this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertFalse(response.get(0).isProductApproved());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
  }

  @Test
   void convertProductsToDistributionProductResponse_allImageViolationsTest() {
    productPage.getContent().get(0).setReviewType(ReviewType.CONTENT);
    productPage.getContent().get(0).setRestrictedKeywordsPresent(true);
    productPage.getContent().get(0).setImageViolations(Arrays.stream(ProductLabels.values())
      .filter(productLabels -> !ProductLabels.PENDING.equals(productLabels))
      .map(ProductLabels::getDescription).collect(Collectors.joining(SolrConstants.COMMA)));
    List<DistributionProductResponse> response =
      this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
    Assertions.assertEquals(10, response.get(0).getImageViolations().size());
  }

  @Test
   void convertProductsToDistributionProductResponse_pendingImageViolationsTest() {
    productPage.getContent().get(0).setReviewType(ReviewType.CONTENT);
    productPage.getContent().get(0).setRestrictedKeywordsPresent(true);
    productPage.getContent().get(0).setImageViolations(ProductLabels.PENDING.getDescription());
    List<DistributionProductResponse> response =
      this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
    Assertions.assertTrue(CollectionUtils.isEmpty(response.get(0).getImageViolations()));
  }

  @Test
   void convertProductsToDistributionProductResponse_invalidImageViolationsTest() {
    productPage.getContent().get(0).setReviewType(ReviewType.CONTENT);
    productPage.getContent().get(0).setImageViolations(STOREID);
    List<DistributionProductResponse> response =
      this.productConverterUtil.convertProductsToDistributionProductResponse(productPage);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getAbleToReject());
    Assertions.assertTrue(response.get(0).getCurrentVendor().getQcRequired());
    Assertions.assertEquals(response.get(0).getCreatedBy(), CREATED_BY);
    Assertions.assertEquals(1, response.get(0).getImageViolations().size());
    Assertions.assertEquals(ProductLabels.GOOD.getDescription(),
        response.get(0).getImageViolations().get(0));
  }

  @Test
   void setReviewerDetailsTest() {
    DistributionProductDetailResponse response = new DistributionProductDetailResponse();
    productConverterUtil.setReviewerDetails(
      ProductReviewer.builder().approverAssignee(DEFAULT_CONTENT_ASSIGNEE).approvedDate(new Date())
        .build(), response);
    Assertions.assertEquals(DEFAULT_CONTENT_ASSIGNEE, response.getProductApproverAssignee());
  }

  @Test
   void getOrderedTextAndImageViolationsTest() {
    product.setImageViolations(BLUR + "," + GOOGLE_RESTRICTED);
    product.setTextViolations(GOOGLE_RESTRICTED);
    List<String> violation = productConverterUtil.getOrderedTextAndImageViolations(product, ORDERED_VIOLATION);
    Assertions.assertEquals(2, violation.size());
    Assertions.assertEquals(GOOGLE_RESTRICTED, violation.get(0));
    Assertions.assertEquals(BLUR, violation.get(1));
  }

}
