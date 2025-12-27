package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.AddProductToIprSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.rest.model.response.EvidenceSubmittedDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.ReportProduct;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import com.gdn.x.mta.distributiontask.util.NotesAndRejectReason;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.springframework.util.CollectionUtils;

public class ConverterUtilTest {

  private static final String STORE_ID = "storeId";
  private static final String MEMBER_ID = "memberId";
  private static final String ITEM_SKU = "itemSku";
  private static final String REASON = "reason";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_DESCRIPTION = "PRODUCT_DESCRIPTION";
  private static final String CATEGORY_CODE1 = "CATEGORY_CODE1";
  private static final String CATEGORY_CODE2 = "CATEGORY_CODE2";
  private static final String CATEGORY_NAME1 = "CATEGORY_NAME1";
  private static final String CATEGORY_NAME2 = "CATEGORY_NAME2";
  private static final String ATTRIBUTE_CODE1 = "ATTRIBUTE_CODE1";
  private static final String ATTRIBUTE_CODE2 = "ATTRIBUTE_CODE2";
  private static final String ATTRIBUTE_CODE3 = "ATTRIBUTE_CODE3";
  private static final String ATTRIBUTE_CODE4 = "ATTRIBUTE_CODE4";
  private static final String ATTRIBUTE_CODE5 = "ATTRIBUTE_CODE5";
  private static final String ATTRIBUTE_CODE6 = "ATTRIBUTE_CODE6";
  private static final String ATTRIBUTE_CODE7 = "ATTRIBUTE_CODE7";
  private static final String ATTRIBUTE_CODE8 = "ATTRIBUTE_CODE8";
  private static final String ATTRIBUTE_CODE9 = "ATTRIBUTE_CODE9";
  private static final String ATTRIBUTE_CODE10 = "ATTRIBUTE_CODE10";
  private static final String ATTRIBUTE_VALUE1 = "ATTRIBUTE_VALUE1";
  private static final String ATTRIBUTE_VALUE2 = "ATTRIBUTE_VALUE2";
  private static final String ATTRIBUTE_VALUE3 = "ATTRIBUTE_VALUE3";
  private static final String ATTRIBUTE_VALUE4 = "ATTRIBUTE_VALUE4";
  private static final String ATTRIBUTE_TYPE1 = "ATTRIBUTE_TYPE1";
  private static final String LOCATION_PATH1 = "LOCATION_PATH1";
  private static final String LOCATION_PATH2 = "LOCATION_PATH2";
  private static final String LOCATION_PATH3 = "LOCATION_PATH3";
  private static final String LOCATION_PATH4 = "LOCATION_PATH4";
  private static final String LOCATION_PATH5 = "LOCATION_PATH5";
  private static final String ITEM_SKU_CODE1 = "ITEM_SKU_CODE1";
  private static final String ITEM_NAME1 = "ITEM_NAME1";
  private static final String UPC_CODE1 = "UPC_CODE1";
  private static final Integer DANGEROUS_GOOD_LEVEL1 = 0;
  private static final String BP_CODE = "BP_CODE";
  private static final String BP_NAME = "BP_NAME";
  private static final String DESCRIPTION = "DESCRIPTION";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "Approved";
  private static final String IMAGE_VIOLATION = "IMAGE_VIOLATION";
  private static final int PREDICTION_SCORE = 1;
  private static final Integer FIVE = 5;
  private static final String NOTES = "NOTES";
  private static final String FAULTY_TYPE = "faultyType";
  private static final String TIME_FILTER_WEB_TYPE = "timeFilterWebType";
  private static final String CHANGED_FIELDS = "{\"isOfficialSeller\": {\"oldValue\": true, \"newValue\" : false}}";
  private static final String CHANGED_FIELDS_2 =
      "{\"isOfficialSeller\":{\"oldValue\":\"true\",\"newValue\":\"false\"}}";
  private static final String RESET_LAST_UPDATE_EMAIL_ADDRESS = "test@example.com";
  private static final String RESET_LAST_UPDATE_EMAIL_ADDRESS_CC = "cc@example.com";
  private static final String MESSAGE = "Your reset message";

  private ProductDetailResponse productDetailResponse;
  private Product existingProduct;
  private AddEditedProductToPDTEvent addEditedProductToPDTEvent;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private AddRevisedProductToPDTEvent addRevisedProductToPDTEvent;
  private ProductRetryStatusUpdate productRetryStatusUpdate;
  private BoostedProductFilterRequest boostedProductFilterRequest;
  private IPRProductSolr iprProductSolr;

  @BeforeEach
  public void setUp(){
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setForReview(true);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setDescription(PRODUCT_DESCRIPTION.getBytes());

    ProductCategoryResponse productCategoryResponse1 = new ProductCategoryResponse();
    CategoryResponse categoryResponse1 = new CategoryResponse();
    productCategoryResponse1.setMarkForDelete(false);
    categoryResponse1.setName(CATEGORY_NAME1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE1);
    productCategoryResponse1.setCategory(categoryResponse1);
    ProductCategoryResponse productCategoryResponse2 = new ProductCategoryResponse();
    CategoryResponse categoryResponse2 = new CategoryResponse();
    productCategoryResponse2.setMarkForDelete(true);
    categoryResponse2.setName(CATEGORY_NAME2);
    categoryResponse2.setCategoryCode(CATEGORY_CODE2);
    productCategoryResponse2.setCategory(categoryResponse2);
    productDetailResponse.setProductCategoryResponses(
        Arrays.asList(productCategoryResponse2, productCategoryResponse1));

    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setMarkForDelete(false);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE1);
    productAttributeResponse1.setAttribute(attributeResponse1);
    ProductAttributeValueResponse productAttributeValueResponse1 = new ProductAttributeValueResponse();
    AllowedAttributeValueResponse allowedAttributeValue = new AllowedAttributeValueResponse();
    allowedAttributeValue.setAllowedAttributeCode(ATTRIBUTE_CODE1);
    allowedAttributeValue.setValue(ATTRIBUTE_VALUE1);
    productAttributeValueResponse1.setAllowedAttributeValue(allowedAttributeValue);
    productAttributeValueResponse1.setMarkForDelete(false);
    productAttributeResponse1.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse1));
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setMarkForDelete(false);
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE2);
    productAttributeResponse2.setAttribute(attributeResponse2);
    ProductAttributeValueResponse productAttributeValueResponse2 = new ProductAttributeValueResponse();
    productAttributeValueResponse2.setDescriptiveAttributeValue(DESCRIPTION);
    productAttributeValueResponse2.setMarkForDelete(false);
    productAttributeResponse2.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse2));
    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setMarkForDelete(false);
    AttributeResponse attributeResponse3 = new AttributeResponse();
    attributeResponse3.setAttributeCode(ATTRIBUTE_CODE3);
    productAttributeResponse3.setAttribute(attributeResponse3);
    ProductAttributeValueResponse productAttributeValueResponse3 = new ProductAttributeValueResponse();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE3);
    predefinedAllowedAttributeValueResponse.setValue(ATTRIBUTE_VALUE2);
    productAttributeValueResponse3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse3.setMarkForDelete(false);
    productAttributeResponse3.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse3));
    ProductAttributeResponse productAttributeResponse4 = new ProductAttributeResponse();
    productAttributeResponse4.setMarkForDelete(false);
    AttributeResponse attributeResponse4 = new AttributeResponse();
    attributeResponse4.setAttributeCode(ATTRIBUTE_CODE4);
    productAttributeResponse4.setAttribute(attributeResponse4);
    ProductAttributeValueResponse productAttributeValueResponse4 = new ProductAttributeValueResponse();
    productAttributeValueResponse4.setMarkForDelete(false);
    productAttributeValueResponse4.setPredefinedAllowedAttributeValue(null);
    productAttributeResponse4.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse4));
    ProductAttributeResponse productAttributeResponse5 = new ProductAttributeResponse();
    productAttributeResponse5.setMarkForDelete(false);
    AttributeResponse attributeResponse5 = new AttributeResponse();
    attributeResponse5.setAttributeCode(ATTRIBUTE_CODE5);
    productAttributeResponse5.setAttribute(attributeResponse5);
    ProductAttributeValueResponse productAttributeValueResponse5 = new ProductAttributeValueResponse();
    productAttributeValueResponse5.setMarkForDelete(true);
    AllowedAttributeValueResponse allowedAttributeValue2 = new AllowedAttributeValueResponse();
    allowedAttributeValue2.setAllowedAttributeCode(ATTRIBUTE_CODE5);
    allowedAttributeValue2.setValue(ATTRIBUTE_VALUE3);
    productAttributeValueResponse5.setAllowedAttributeValue(allowedAttributeValue2);
    productAttributeResponse5.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse5));
    ProductAttributeResponse productAttributeResponse6 = new ProductAttributeResponse();
    productAttributeResponse6.setMarkForDelete(false);
    AttributeResponse attributeResponse6 = new AttributeResponse();
    attributeResponse6.setAttributeCode(ATTRIBUTE_CODE9);
    attributeResponse6.setAttributeType("PREDEFINED_ATTRIBUTE");
    productAttributeResponse6.setAttribute(attributeResponse6);
    ProductAttributeValueResponse productAttributeValueResponse6 = new ProductAttributeValueResponse();
    productAttributeValueResponse6.setMarkForDelete(false);
    productAttributeValueResponse6.setPredefinedAllowedAttributeValue(null);
    productAttributeResponse6.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse6));
    ProductAttributeResponse productAttributeResponse7 = new ProductAttributeResponse();
    productAttributeResponse7.setMarkForDelete(false);
    AttributeResponse attributeResponse7 = new AttributeResponse();
    attributeResponse7.setAttributeCode(ATTRIBUTE_CODE10);
    attributeResponse7.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    productAttributeResponse7.setAttribute(attributeResponse7);
    ProductAttributeValueResponse productAttributeValueResponse7 = new ProductAttributeValueResponse();
    productAttributeValueResponse7.setDescriptiveAttributeValue(null);
    productAttributeValueResponse7.setMarkForDelete(false);
    productAttributeResponse7.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse7));
    productDetailResponse.setProductAttributeResponses(
        Arrays.asList(productAttributeResponse1, productAttributeResponse2, productAttributeResponse3,
            productAttributeResponse4, productAttributeResponse5, productAttributeResponse6, productAttributeResponse7));

    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setMarkForDelete(false);
    productItemResponse1.setSkuCode(ITEM_SKU_CODE1);
    productItemResponse1.setGeneratedItemName(ITEM_NAME1);
    productItemResponse1.setUpcCode(UPC_CODE1);
    productItemResponse1.setDangerousGoodsLevel(DANGEROUS_GOOD_LEVEL1);
    Image productItemImage1 = new Image(true, LOCATION_PATH1, 0);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setOriginalImage(Boolean.TRUE);
    Image productItemImage2 = new Image(true, LOCATION_PATH5, 0);
    productItemImage2.setMarkForDelete(true);
    productItemResponse1.setImages(Arrays.asList(productItemImage1, productItemImage2));
    ProductItemAttributeValueResponse productItemAttributeValueResponse1 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse1.setValue(ATTRIBUTE_VALUE4);
    productItemAttributeValueResponse1.setMarkForDelete(false);
    AttributeResponse itemAttributeResponse1 = new AttributeResponse();
    itemAttributeResponse1.setAttributeCode(ATTRIBUTE_CODE6);
    itemAttributeResponse1.setAttributeType(ATTRIBUTE_TYPE1);
    productItemAttributeValueResponse1.setAttributeResponse(itemAttributeResponse1);
    ProductItemAttributeValueResponse productItemAttributeValueResponse2 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse2.setMarkForDelete(true);
    AttributeResponse itemAttributeResponse2 = new AttributeResponse();
    itemAttributeResponse2.setAttributeCode(ATTRIBUTE_CODE7);
    productItemAttributeValueResponse2.setAttributeResponse(itemAttributeResponse2);
    productItemResponse1.setProductItemAttributeValueResponses(
        Arrays.asList(productItemAttributeValueResponse1, productItemAttributeValueResponse2));

    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setMarkForDelete(true);
    Image productItemImage3 = new Image(true, LOCATION_PATH2, 0);
    productItemImage3.setOriginalImage(Boolean.TRUE);
    productItemResponse2.setImages(Collections.singletonList(productItemImage3));
    ProductItemAttributeValueResponse productItemAttributeValueResponse3 = new ProductItemAttributeValueResponse();
    AttributeResponse itemAttributeResponse3 = new AttributeResponse();
    itemAttributeResponse3.setAttributeCode(ATTRIBUTE_CODE8);
    productItemAttributeValueResponse3.setAttributeResponse(itemAttributeResponse2);
    productItemResponse2.setProductItemAttributeValueResponses(
        Collections.singletonList(productItemAttributeValueResponse3));

    productDetailResponse.setProductItemResponses(new HashSet<>());
    productDetailResponse.getProductItemResponses().add(productItemResponse1);
    productDetailResponse.getProductItemResponses().add(productItemResponse2);
    productDetailResponse.setPromoSKU(Boolean.TRUE);

    Image productImage1 = new Image(true, LOCATION_PATH3, 0);
    productImage1.setMarkForDelete(false);
    productImage1.setOriginalImage(Boolean.TRUE);
    Image productImage2 = new Image(true, LOCATION_PATH4, 0);
    productImage2.setMarkForDelete(true);

    productDetailResponse.setImages(Arrays.asList(productImage1, productImage2));
    existingProduct = new Product();
    existingProduct.setBusinessPartnerName(BP_NAME);
    existingProduct.setBusinessPartnerCode(BP_CODE);
    existingProduct.setPostLive(true);
    existingProduct.setRestrictedKeywordsPresent(true);
    existingProduct.setBrandCode(BRAND_CODE);
    existingProduct.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    existingProduct.setImageViolations(IMAGE_VIOLATION);
    existingProduct.setProductPredictionScore(PREDICTION_SCORE);

    addEditedProductToPDTEvent = new AddEditedProductToPDTEvent();
    addEditedProductToPDTEvent.setB2bActivated(true);
    addEditedProductToPDTEvent.setB2cActivated(true);
    addRevisedProductToPDTEvent = new AddRevisedProductToPDTEvent();
    addRevisedProductToPDTEvent.setPostLive(true);
    addRevisedProductToPDTEvent.setB2bActivated(true);
    addRevisedProductToPDTEvent.setB2cActivated(true);

    productRetryStatusUpdate =
      ProductRetryStatusUpdate.builder().state(WorkflowState.PASSED.name()).edited(Boolean.FALSE)
        .reviewType(ReviewType.CONTENT.name()).revised(Boolean.TRUE).markForDelete(Boolean.TRUE)
        .build();
    boostedProductFilterRequest = new BoostedProductFilterRequest();

    iprProductSolr = new IPRProductSolr();
  }

  @Test
   void convertToReportProduct() {
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(ITEM_SKU).reason(REASON).build();
    ReportProduct reportProduct = ConverterUtil.convertToReportProduct(STORE_ID, reportProductRequest);
    Assertions.assertEquals(MEMBER_ID, reportProduct.getMemberId());
    Assertions.assertEquals(STORE_ID, reportProduct.getStoreId());
    Assertions.assertEquals(ITEM_SKU, reportProduct.getItemSku());
    Assertions.assertEquals(REASON, reportProduct.getReason());
    Assertions.assertFalse(reportProduct.isMarkForDelete());
  }

  @Test
   void convertProductDetailResponseToProduct() throws JsonProcessingException {
    productDetailResponse.getImages().forEach(image -> image.setCommonImage(true));
    productDetailResponse.getProductItemResponses()
        .forEach(item -> item.getImages().forEach(image -> image.setCommonImage(true)));
    Product product = ConverterUtil
        .convertProductDetailResponseToProduct(productDetailResponse, addEditedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Assertions.assertEquals(PRODUCT_CODE, product.getProductCode());
    Assertions.assertFalse(
        product.getProductImages().stream().anyMatch(image -> !image.isCommonImage()));
    Assertions.assertFalse(
        product.getProductItems().stream().flatMap(item -> item.getProductItemImages().stream())
            .anyMatch(image -> !image.isCommonImage()));
    Assertions.assertTrue(product.isB2bActivated());
    Assertions.assertTrue(product.isB2cActivated());
  }

  @Test
   void convertProductDetailResponseToProductTest() throws JsonProcessingException {
    Product product = ConverterUtil
        .convertProductDetailResponseToProduct(productDetailResponse, addRevisedProductToPDTEvent,
            imageQcProcessedAndBrandResponse);
    Assertions.assertEquals(PRODUCT_CODE, product.getProductCode());
    Assertions.assertTrue(product.isPostLive());
    Assertions.assertTrue(product.isB2cActivated());
    Assertions.assertTrue(product.isB2bActivated());
  }

  @Test
   void getProductHistoryRequestForAutoApprovalTest() {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setState(WorkflowState.PASSED);
    product.setStoreId(STORE_ID);
    ProductHistoryRequest productHistoryRequest =
        ConverterUtil.getProductHistoryRequestForAutoApproval(Constants.CONTENT_AND_IMAGE_AUTO_APPROVAL,
            Constants.AUTO_APPROVED, product);
    Assertions.assertEquals(PRODUCT_CODE, productHistoryRequest.getProductCode());;
    Assertions.assertEquals(FIVE, productHistoryRequest.getState());
    Assertions.assertEquals(Constants.CONTENT_AND_IMAGE_AUTO_APPROVAL,
        productHistoryRequest.getNotes());
    Assertions.assertEquals(Constants.SYSTEM, productHistoryRequest.getCreatedBy());
    Assertions.assertEquals(Constants.SYSTEM, productHistoryRequest.getUpdatedBy());
    Assertions.assertEquals(STORE_ID, productHistoryRequest.getStoreId());
    Assertions.assertEquals(Constants.AUTO_APPROVED, productHistoryRequest.getDescription());
    Assertions.assertNotNull(productHistoryRequest.getCreatedDate());
    Assertions.assertNotNull(productHistoryRequest.getUpdatedDate());
  }

  @Test
   void setReviewAutoApprovalDetailsTest() {
    Date date = new Date();
    ProductReviewer productReviewer = new ProductReviewer();
    ConverterUtil.setReviewAutoApprovalDetails(date, productReviewer);
    Assertions.assertEquals(date, productReviewer.getApprovedDate());
    Assertions.assertEquals(date, productReviewer.getAssignedDate());
    Assertions.assertEquals(Constants.AUTO_APPROVED, productReviewer.getApproverAssignee());
  }

  @Test
   void toPDTAutoApprovalEventModelTest() {
    PDTAutoApprovalEventModel pdtAutoApprovalEventModel = ConverterUtil.toPDTAutoApprovalEventModel(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, pdtAutoApprovalEventModel.getProductCode());
  }

  @Test
   void toNotesAndRejectReason() {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode(PRODUCT_CODE);
    rejectProductDTO.setNotes(NOTES);
    RejectReasonDto rejectReasonDto = new RejectReasonDto();
    List<String> products = new ArrayList<>();
    products.add("TestProduct");
    rejectReasonDto.setProduct(products);
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);
    NotesAndRejectReason notesAndRejectReason = ConverterUtil.getNotesAndRejectReason(rejectProductDTO);
    Assertions.assertEquals(NOTES, notesAndRejectReason.getNotes());
    Assertions.assertEquals("TestProduct",
        notesAndRejectReason.getRejectReasonRequest().getProduct().get(0));
  }

  @Test
   void toProductAutoApprovalTest() {
    ProductAutoApproval productAutoApproval = ConverterUtil.toProductAutoApproval(existingProduct);
    Assertions.assertEquals(existingProduct.getProductCode(), productAutoApproval.getProductCode());
    Assertions.assertEquals(AutoApprovalStatus.PENDING,
        productAutoApproval.getAutoApprovalStatus());
    Assertions.assertEquals(0, productAutoApproval.getRetryCount());
  }

  @Test
   void toAutoQcConfigChangeRequestTest() throws IOException {
    AutoQcConfigChange autoQcConfigChange = new AutoQcConfigChange();
    autoQcConfigChange.setSellerCode(BP_CODE);
    autoQcConfigChange.setC1CategoryCode(CATEGORY_CODE1);
    autoQcConfigChange.setChangedFields(CHANGED_FIELDS);
    AutoQcConfigChangeRequest autoQcConfigChangeRequest = ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChange);
    Assertions.assertEquals(BP_CODE, autoQcConfigChangeRequest.getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE1, autoQcConfigChangeRequest.getCategoryCode());
    Assertions.assertEquals(CHANGED_FIELDS_2,
        new ObjectMapper().writeValueAsString(autoQcConfigChangeRequest.getChangedFields()));
  }

  @Test
   void setProductRetryStatus_emptyRequestTest() {
    Product product = ConverterUtil.setProductRetryStatus(new Product(),
      new ProductRetryStatusUpdate());
    Assertions.assertNull(product.getState());
  }

  @Test
   void setProductRetryStatusTest() {
    Product product = ConverterUtil.setProductRetryStatus(new Product(),
      productRetryStatusUpdate);
    Assertions.assertEquals(WorkflowState.PASSED, product.getState());
    Assertions.assertEquals(ReviewType.CONTENT, product.getReviewType());
    Assertions.assertFalse(product.isEdited());
    Assertions.assertTrue(product.isRevised());
  }

  @Test
   void convertBoostedFilterRequestToPrimaryFilterDTOTest() {
    boostedProductFilterRequest.setTimeFilterWebType(TIME_FILTER_WEB_TYPE);
    boostedProductFilterRequest.setFaultyType(FAULTY_TYPE);
    SummaryFilterDTO summaryFilterDTO =
        ConverterUtil.convertBoostedFilterRequestToPrimaryFilterDTO(boostedProductFilterRequest);
    Assertions.assertEquals(summaryFilterDTO.getTimeFilterType(),
        TimeFilterType.getTimeFilterTypeByValue(boostedProductFilterRequest.getTimeFilterWebType()));
    Assertions.assertEquals(summaryFilterDTO.getFaultyImageType(),
        boostedProductFilterRequest.getFaultyType());
  }

  @Test
   void convertBoostedFilterRequestToPrimaryFilterDTOFalseTest() {
    boostedProductFilterRequest.setTimeFilterWebType(TIME_FILTER_WEB_TYPE);
    SummaryFilterDTO summaryFilterDTO =
        ConverterUtil.convertBoostedFilterRequestToPrimaryFilterDTO(boostedProductFilterRequest);
    Assertions.assertEquals(summaryFilterDTO.getTimeFilterType(),
        TimeFilterType.getTimeFilterTypeByValue(boostedProductFilterRequest.getTimeFilterWebType()));
  }

  @Test
   void isCategoryDataToBeUpdatedTest() {
    ConverterUtil.isCategoryDataToBeUpdated(PRODUCT_CODE, null, new Product());
    AutoApprovalTypeResponse autoApprovalTypeResponse = new AutoApprovalTypeResponse();
    ConverterUtil.isCategoryDataToBeUpdated(PRODUCT_CODE, autoApprovalTypeResponse, new Product());
    autoApprovalTypeResponse.setCategoryCode(CATEGORY_CODE1);
    ConverterUtil.isCategoryDataToBeUpdated(PRODUCT_CODE, autoApprovalTypeResponse, new Product());
    autoApprovalTypeResponse.setCategoryName(CATEGORY_CODE1);
    ConverterUtil.isCategoryDataToBeUpdated(PRODUCT_CODE, autoApprovalTypeResponse, new Product());
    Product product = new Product();
    product.setCategoryCode(CATEGORY_CODE1);
    ConverterUtil.isCategoryDataToBeUpdated(PRODUCT_CODE, autoApprovalTypeResponse, product);
  }

  @Test
   void convertToInternalHistoryModelTest() {
    InternalHistoryEventModel eventModel =
      ConverterUtil.convertToInternalHistoryModel(STORE_ID, PRODUCT_CODE, "USERNAME", "ACTIVITY",
        "NOTES");
    Assertions.assertEquals(eventModel.getProductCode(), PRODUCT_CODE);
  }

  @Test
   void testGetResetDeltaReindexMessageEmailRequest() {
    MessageEmailRequest emailRequest = ConverterUtil.getResetDeltaReindexMessageEmailRequest(
            RESET_LAST_UPDATE_EMAIL_ADDRESS, RESET_LAST_UPDATE_EMAIL_ADDRESS_CC, MESSAGE);
    Assertions.assertNotNull(emailRequest);
    Assertions.assertEquals(Constants.RESET_LAST_UPDATED_DATE_DELTA_REINDEX_TEMPLATE_ID,
        emailRequest.getMessageId());
    Assertions.assertEquals(RESET_LAST_UPDATE_EMAIL_ADDRESS, emailRequest.getMessageTo());
    Assertions.assertEquals(RESET_LAST_UPDATE_EMAIL_ADDRESS_CC, emailRequest.getMessageCc());
    Assertions.assertEquals(Constants.RESET_LAST_UPDATED_DATE_DELTA_REINDEX_TEMPLATE_ID,
        emailRequest.getMessageIdentifierKey());
    Assertions.assertNotNull(emailRequest.getMessageIdentifierValue());
    Assertions.assertNotNull(emailRequest.getVariables());
  }

  @Test
  public void constructHistoricalDataResponseTest() {
    SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
    sellerAnalyticsResponse.setUnauthorizedResellerProductCount(1);
    sellerAnalyticsResponse.setSuspiciousSkuProductCount(1);
    sellerAnalyticsResponse.setProhibitedProductCount(1);
    sellerAnalyticsResponse.setRejectedProductCount(1);
    sellerAnalyticsResponse.setExpiredProductCount(1);
    ConverterUtil.constructHistoricalDataResponse(sellerAnalyticsResponse);
  }

  @Test
  public void populateAssigneeToIprFromDbTest() {
    iprProductSolr.setAssignedTo("username");
    iprProductSolr.setProductSku(PRODUCT_CODE);
    Map<String, String> productSkuAssigneeMap = new HashMap<>();
    productSkuAssigneeMap.put(PRODUCT_CODE, "Username-2");
    List<IPRProductSolr> result =
      ConverterUtil.populateAssigneeToIprFromDb(Collections.singletonList(iprProductSolr),
        productSkuAssigneeMap);
    Assertions.assertEquals("Username-2", result.get(0).getAssignedTo());
  }

  @Test
  public void populateAssigneeToIprFromDbDataNotPresetTest() {
    iprProductSolr.setAssignedTo("username");
    iprProductSolr.setProductSku(PRODUCT_CODE);
    Map<String, String> productSkuAssigneeMap = new HashMap<>();
    productSkuAssigneeMap.put(PRODUCT_NAME, "Username-2");
    List<IPRProductSolr> result =
      ConverterUtil.populateAssigneeToIprFromDb(Collections.singletonList(iprProductSolr),
        productSkuAssigneeMap);
    Assertions.assertEquals("username", result.get(0).getAssignedTo());
  }

  @Test
  public void toAddProductToIprSolrEventModelWaitingToGetActivatedTest(){
    ProductIPR productIPR = new ProductIPR();
    productIPR.setProductSku(PRODUCT_CODE);
    productIPR.setState(ProductStateIPR.WAITING_TO_GET_ACTIVATED.name());
    AddProductToIprSolrEventModel eventModel =
      ConverterUtil.toAddProductToIprSolrEventModel(productIPR);
    Assertions.assertEquals(PRODUCT_CODE, eventModel.getProductSku());
    Assertions.assertNull(eventModel.getIprProductSolr());
  }

  @Test
  public void toAddProductToIprSolrEventModelTest(){
    ProductIPR productIPR = new ProductIPR();
    productIPR.setProductSku(PRODUCT_CODE);
    productIPR.setState(ProductStateIPR.IN_REVIEW.name());
    AddProductToIprSolrEventModel eventModel =
      ConverterUtil.toAddProductToIprSolrEventModel(productIPR);
    Assertions.assertEquals(PRODUCT_CODE, eventModel.getProductSku());
    Assertions.assertNotNull(eventModel.getIprProductSolr());
  }

  @Test
  void toProductEmailEventModelTest() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setSellerNotes(NOTES);
    ProductEmailEventModel productEmailEventModel =
      ConverterUtil.toProductEmailEventModel(productIPR);
    Assertions.assertEquals(Constants.EVIDENCE_REQUESTED_MAIL_TYPE,
      productEmailEventModel.getProductEmailType());
  }

  @Test
  void processEventForProductEmailExistingEvidenceRequestedTest() {
    ProductEmailEventModel eventModel =
      ConverterUtil.processEventForProductEmail(ProductStateIPR.EVIDENCE_REQUESTED.name(),
        ProductStateIPR.EVIDENCE_SUBMITTED.name(), new ProductIPR());
    Assertions.assertTrue(eventModel.isResetStatus());
  }

  @Test
  void processEventForProductEmailNoChangeInEvidenceRequestedTest() {
    ProductEmailEventModel eventModel =
      ConverterUtil.processEventForProductEmail(ProductStateIPR.EVIDENCE_REQUESTED.name(),
        ProductStateIPR.EVIDENCE_REQUESTED.name(), new ProductIPR());
    Assertions.assertNull(eventModel);
  }

  @Test
  void processEventForProductEmailExistingInReviewTest() {
    ProductEmailEventModel eventModel =
      ConverterUtil.processEventForProductEmail(ProductStateIPR.IN_REVIEW.name(),
        ProductStateIPR.EVIDENCE_SUBMITTED.name(), new ProductIPR());
    Assertions.assertNull(eventModel);
  }

  @Test
  void processEventForProductEmailNewEvidenceRequestedTest() {
    ProductEmailEventModel eventModel =
      ConverterUtil.processEventForProductEmail(ProductStateIPR.IN_REVIEW.name(),
        ProductStateIPR.EVIDENCE_REQUESTED.name(), new ProductIPR());
    Assertions.assertNotNull(eventModel);
  }

  @Test
  void constructEvidenceSubmittedDetailResponseTrimTest() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setEvidenceFilePath("");
    productIPR.setEvidenceUrl("");
    EvidenceSubmittedDetailResponse result =
      ConverterUtil.constructEvidenceSubmittedDetailResponse(productIPR);
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getEvidenceUrl()));
  }

  @Test
  void constructEvidenceSubmittedDetailResponseTest() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setEvidenceFilePath("file");
    productIPR.setEvidenceUrl("url");
    EvidenceSubmittedDetailResponse result =
      ConverterUtil.constructEvidenceSubmittedDetailResponse(productIPR);
    Assertions.assertTrue(!CollectionUtils.isEmpty(result.getEvidenceUrl()));
  }

  @Test
  void constructBrandReportDetailResponseTest() {
    SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
    ProductIPR productIPR = new ProductIPR();
    productIPR.setSource(ProductSourceIPR.BRAND_REPORT.getValue());
    productIPR.setBrandName("brand");
    productIPR.setReporterEmail("email");
    productIPR.setReporterReason("reason");
    productIPR.setReportDate(new Date(2025, 1, 1));
    productIPR.setReporterName("name");
    IprProductDetailsResponse result =
        ConverterUtil.constructIPRProductDetailResponse(productIPR, sellerAnalyticsResponse);
    Assertions.assertEquals(result.getBrandReportDetailResponse().getReporterEmail(), "email");
  }

  @Test
  void constructBrandReportDetailResponseNotBrandSourceTest() {
    SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
    ProductIPR productIPR = new ProductIPR();
    IprProductDetailsResponse result =
        ConverterUtil.constructIPRProductDetailResponse(productIPR, sellerAnalyticsResponse);
    Assertions.assertEquals(result.getBrandReportDetailResponse().getReporterEmail(), null);
  }

  @Test
  void constructIPRProductDetailResponseWithEmptySellerHistoricalDataTest() {
    SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
    ProductIPR productIPR = new ProductIPR();
    IprProductDetailsResponse result =
        ConverterUtil.constructIPRProductDetailResponseWithEmptySellerHistoricalData(productIPR);
    Assertions.assertEquals(result.getBrandReportDetailResponse().getReporterEmail(), null);
  }

  @Test
  void constructIPRProductDetailResponseWithEmptySellerHistoricalAndNoBrandReportTest() {
    ProductIPR productIPR = new ProductIPR();
    productIPR.setSource(ProductSourceIPR.BRAND_REPORT.getValue());
    productIPR.setBrandName("brand");
    productIPR.setReporterEmail("email");
    productIPR.setReporterReason("reason");
    productIPR.setReportDate(new Date(2025, 1, 1));
    productIPR.setReporterName("name");
    IprProductDetailsResponse result =
        ConverterUtil.constructIPRProductDetailResponseWithEmptySellerHistoricalData(productIPR);
    Assertions.assertEquals(result.getBrandReportDetailResponse().getReporterEmail(), "email");
  }

  @Test
  void convertSourceToSetNullSource() {
    Assertions.assertTrue(ConverterUtil.convertSourceToSet(null).isEmpty());
  }

}
