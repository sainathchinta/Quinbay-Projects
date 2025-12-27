package com.gdn.x.mta.distributiontask.controller.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.type.DifficultyLevel;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductItemRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;

/**
 * Created by Alok on 10/11/16.
 */
public class ModelConverterUtilTest {

  private static final List<String> PRODUCT_CODES = new ArrayList<>();
  private static final String PRODUCT_CODE = "productCode";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String BRAND = "Brand";
  private static final String UKURAN = "Ukuran";
  private static final String BLIBLI = "Blibli.com";
  private static final String VALUE = "35";
  private static final String LOCATION_PATH = "location-path";
  private static final String SKU_CODE = "sku-code";
  private static final String CONTENT = "content";
  private static final String IMAGE = "image";
  private static final String NOT_APPLICABLE  = "NA";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private static final String NOTES = "NOTES";
  private static final String PRODUCT = "PRODUCT";

  private BulkVendorProductActionsRequest bulkVendorProductActionsRequest;
  private BulkScreeningProductActionsRequest bulkScreeningProductActionsRequest;
  private DistributionProductDetailRequest distributionProductDetailRequest;
  private List<DistributionProductAttributeRequest> distributionProductAttributeRequestList;

  @InjectMocks
  ModelConverterUtil modelConverterUtil;

  @Mock
  ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    PRODUCT_CODES.add(PRODUCT_CODE);
    bulkScreeningProductActionsRequest = new BulkScreeningProductActionsRequest();
    bulkScreeningProductActionsRequest.setProductCodes(PRODUCT_CODES);
    bulkScreeningProductActionsRequest.setAssignedBy(ASSIGNED_BY);
    bulkScreeningProductActionsRequest.setAssignTo(ASSIGNED_TO);

    bulkVendorProductActionsRequest = new BulkVendorProductActionsRequest();
    bulkVendorProductActionsRequest.setBulkScreeningProductActionsRequests(Collections
        .singletonList(bulkScreeningProductActionsRequest));

    distributionProductDetailRequest = new DistributionProductDetailRequest();
    distributionProductAttributeRequestList =
        new ArrayList<>();
    distributionProductAttributeRequestList.add(new DistributionProductAttributeRequest(ATTRIBUTE_CODE, BRAND, BLIBLI,
        AttributeType.PREDEFINED_ATTRIBUTE.name()));
    distributionProductAttributeRequestList.add(new DistributionProductAttributeRequest(ATTRIBUTE_CODE, UKURAN, VALUE,
        AttributeType.DEFINING_ATTRIBUTE.name()));
    distributionProductAttributeRequestList.add(new DistributionProductAttributeRequest(ATTRIBUTE_CODE, BRAND, BLIBLI,
        AttributeType.DEFINING_ATTRIBUTE.name()));
    distributionProductDetailRequest.setProductAttributes(distributionProductAttributeRequestList);
    List<DistributionProductImageRequest> distributionProductImageRequestList = new ArrayList<>();
    distributionProductImageRequestList.add(
        new DistributionProductImageRequest(LOCATION_PATH, 0, false, null, true, true, false));
    distributionProductImageRequestList.add(
        new DistributionProductImageRequest(LOCATION_PATH, 0, false, true, false, false, false));
    distributionProductDetailRequest.setProductImages(distributionProductImageRequestList);
    distributionProductDetailRequest.setProductImages(distributionProductImageRequestList);
    List<DistributionProductItemRequest> distributionProductItemRequestList = new ArrayList<>();
    DistributionProductItemRequest distributionProductItemRequest =
        new DistributionProductItemRequest();
    distributionProductItemRequest.setSkuCode(SKU_CODE);
    distributionProductItemRequest.setProductItemImages(distributionProductImageRequestList);
    distributionProductItemRequest
        .setProductItemAttributes(distributionProductAttributeRequestList);
    distributionProductItemRequestList.add(distributionProductItemRequest);

    distributionProductDetailRequest.setProductItems(distributionProductItemRequestList);

    distributionProductDetailRequest.setBrandCode(BRAND_CODE);
    distributionProductDetailRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    distributionProductDetailRequest.setEdited(false);
    distributionProductDetailRequest.setProductType(1);
  }

  @Test
   void convertProductDetailRequestToProductEntityTestOk() throws Exception {
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, CONTENT);
    Assertions.assertNull(response.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(0).isEdited());
    Assertions.assertEquals(DifficultyLevel.HIGH, response.getContentDifficultyLevel());
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, response.getBrandApprovalStatus());
  }

  @Test
   void convertProductDetailRequestToProductEntityTestImageOk() throws Exception {
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, IMAGE);
    Assertions.assertNull(response.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(1).getOriginalImage());
    Assertions.assertEquals(DifficultyLevel.HIGH, response.getImageDifficultyLevel());
  }

  @Test
   void convertProductDetailRequestToProductEntityTestNAOk() throws Exception {
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE);
    Assertions.assertNull(response.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(1).getOriginalImage());
    Assertions.assertEquals(DifficultyLevel.NA, response.getImageDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.NA, response.getContentDifficultyLevel());
  }

  @Test
  void convertProductDetailRequestToProductEntityTestAttributesEmpty() throws Exception {
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    distributionProductDetailRequest.getProductItems().get(0).setProductItemAttributes(new ArrayList<>());
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE);
    Assertions.assertNull(response.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(1).getOriginalImage());
    Assertions.assertEquals(DifficultyLevel.NA, response.getImageDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.NA, response.getContentDifficultyLevel());
  }

  @Test
  void convertProductDetailRequestToProductEntityTestEmptyItemImage() throws Exception {
    ReflectionTestUtils.setField(modelConverterUtil, "throwErrorOnEmptyItemImages", true);
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    DistributionProductItemRequest distributionProductItemRequest =
        new DistributionProductItemRequest();
    distributionProductItemRequest.setProductItemImages(new ArrayList<>());
    distributionProductDetailRequest.setProductItems(Collections.singletonList(distributionProductItemRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, CONTENT));
  }
  @Test
  void convertProductDetailRequestToProductEntityTestItemImagePresent() throws Exception {
    ReflectionTestUtils.setField(modelConverterUtil, "throwErrorOnEmptyItemImages", true);
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    DistributionProductItemRequest distributionProductItemRequest =
        new DistributionProductItemRequest();
    List<DistributionProductImageRequest> distributionProductImageRequestList = new ArrayList<>();
    distributionProductImageRequestList.add(
        new DistributionProductImageRequest(LOCATION_PATH, 0, false, null, true, true, false));
    distributionProductItemRequest.setProductItemImages(distributionProductImageRequestList);
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, CONTENT);
  }

  @Test
   void toBulkVendorProductActionsDTOTest() {
    BulkVendorProductActionsDTO bulkVendorProductActionsDTO =
        modelConverterUtil.toBulkVendorProductActionsDTO(bulkVendorProductActionsRequest);
    Assertions.assertEquals(PRODUCT_CODES,
        bulkVendorProductActionsDTO.getBulkScreeningProductActionsRequests().get(0).getProductCodes());
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsDTO.getBulkScreeningProductActionsRequests().get(0).getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsDTO.getBulkScreeningProductActionsRequests().get(0).getAssignTo());
  }

  @Test
   void convertProductDetailRequestToProductEntityTestOkWithException() throws Exception {
    DistributionProductDetailRequest distributionProductDetailRequest = null;
    Assertions.assertThrows(Exception.class,
      () -> modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, IMAGE));
  }

  @Test
   void convertProductDetailRequestToProductEntityVendorNotesTestOk() throws Exception {
    distributionProductDetailRequest.setDifficultyLevel(DifficultyLevel.HIGH.name());
    distributionProductDetailRequest.setProductNotes(
        ProductNotesRequest.builder().allVariants(true).vendorNotes(List.of("notes")).build());
    distributionProductDetailRequest.getProductItems().get(0)
        .setItemNotes(ItemNotesRequest.builder().vendorNotes(List.of("item notes")).build());
    String notes = "{\n" + "    \"vendorNotes\": [\"Incomplete or inappropriate content\"],\n"
        + "    \"vendorErrorFields\": [\"url\", \"description\"],\n" + "    \"contentAdditionalNotes\": \"notes\",\n"
        + "    \"allVariants\": true,\n" + "    \"imagesAdditionalNotes\": \"notes\"}";
    Mockito.when(mapper.writeValueAsString(Mockito.any())).thenReturn(notes);
    Product response =
        modelConverterUtil.convertProductDetailRequestToProductEntity(distributionProductDetailRequest, CONTENT);
    Assertions.assertNull(response.getProductImages().get(0).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(1).getOriginalImage());
    Assertions.assertTrue(response.getProductImages().get(0).isEdited());
    Assertions.assertEquals(DifficultyLevel.HIGH, response.getContentDifficultyLevel());
    Assertions.assertEquals(BRAND_CODE, response.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, response.getBrandApprovalStatus());
    Assertions.assertEquals(notes, response.getProductNotes());
    Assertions.assertEquals(notes, response.getProductItems().get(0).getItemNotes());
  }

  @Test
   void toRejectProductDTOTest() {
    RejectProductVendorRequest rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setNotes(NOTES);
    rejectProductVendorRequest.setBulkAction(true);
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReasonRequest.setProduct(product);
    rejectProductVendorRequest.setRejectReasonRequest(rejectReasonRequest);
    RejectProductDTO rejectProductDTO = ModelConverterUtil.toRejectProductDTO(rejectProductVendorRequest);
    Assertions.assertEquals(PRODUCT_CODE, rejectProductDTO.getProductCode());
    Assertions.assertEquals(NOTES, rejectProductDTO.getNotes());
    Assertions.assertEquals(PRODUCT, rejectProductDTO.getRejectReasonDto().getProduct().get(0));
    Assertions.assertTrue(rejectProductVendorRequest.isBulkAction());
  }

  @AfterEach public void tearDown() throws Exception {

  }
}
