package com.gdn.partners.pcu.internal.web.controller.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TreeMap;

import com.gdn.partners.pcu.internal.client.constants.ClientParameter;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.controller.ImageController;
import com.gdn.partners.pcu.internal.web.controller.ProductController;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthHistoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.NeedRevisionNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectReason;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductItemRequest;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

@ExtendWith(SpringExtension.class)
public class ConverterUtilTest {

  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String ITEM_SKU = "MTA-0000001-0001";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATALOG_CODE = "catalog-code";
  private static final String USER_NAME = "userName";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DESCRIPTION = "brandDescription";
  private static final String URL = "www.test.com";
  private static final String VIDEO_URL = "video_url";
  private static final String IMAGE_LOCATION_PATH = "location-path";
  private static final String ATTRIBUTE_NAME = "Warna";
  private static final String NOTES = "notes";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private static final String PRODUCT = "PRODUCT";
  private static final String SELLER_CODE = "seller-code";
  public static final String ACTIVE = "ACTIVE";
  public static final String IMAGE_FILE_NAME = "image-file-name";
  public static final String ORIGINAL_FILE_NAME = "original-file-name";

  private ProductWebRequest productWebRequest;
  private UpdateBrandWebRequest updateBrandWebRequest;
  private ProductWebRequest productWebRequestForVendorUpdate;
  private BrandAuthCreateWebRequest brandAuthCreateWebRequest;

  @MockBean
  private ClientParameterHelper clientParameterHelper;

  @MockBean
  private ClientParameter clientParameter;

  @MockBean
  private ProductController productController;

  @MockBean
  private ImageController imageController;

  @BeforeEach
  public void setUp() throws Exception {
    productWebRequest = ProductWebRequest.builder()
        .productCode(PRODUCT_CODE)
        .name(PRODUCT_NAME)
        .brand(BRAND_NAME)
        .url(VIDEO_URL)
        .version(2l).edited(false)
        .build();
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode("hash-code");
    imageRequest.setLocationPath(IMAGE_LOCATION_PATH);
    imageRequest.setSequence(1);
    imageRequest.setMainImages(true);
    imageRequest.setEdited(false);

    ImageRequest imageRequest1 = new ImageRequest();
    imageRequest1.setMarkForDelete(true);
    List<ImageRequest> imageRequests = new ArrayList<>();
    imageRequests.add(imageRequest);
    productWebRequest.setImages(imageRequests);
    TreeMap<String, String> attributesMap = new TreeMap<String, String>();
    attributesMap.put("image1", "hash-code1");

    List<ProductCategoryWebRequest> productCategories = new ArrayList<>();
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    productCategoryWebRequest.setMarkForDelete(Boolean.TRUE);
    CatalogWebRequest catalogWebRequest = new CatalogWebRequest();
    catalogWebRequest.setCatalogCode(CATALOG_CODE);
    productCategoryWebRequest.setCatalog(catalogWebRequest);
    productCategories.add(productCategoryWebRequest);
    productWebRequest.setProductCategories(productCategories);

    List<ProductAttributeWebRequest> productAttributes = new ArrayList<>();
    ProductAttributeWebRequest productAttributeWebRequest = new ProductAttributeWebRequest();
    AttributeWebRequest attributeWebRequest = new AttributeWebRequest();
    attributeWebRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeWebRequest.setAttributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE);
    List<AllowedAttributeValueWebRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueWebRequest allowedAttributeValueWebRequest = new AllowedAttributeValueWebRequest();
    allowedAttributeValueWebRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueWebRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueWebRequest);
    attributeWebRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest =
        new PredefinedAllowedAttributeValueWebRequest();
    predefinedAllowedAttributeValueWebRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueWebRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueWebRequest);
    attributeWebRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeWebRequest.setAttribute(attributeWebRequest);
    productAttributeWebRequest.setSequence(2);
    productAttributes.add(productAttributeWebRequest);
    productWebRequest.setProductAttributes(productAttributes);

    List<ProductItemWebRequest> productItems = new ArrayList<>();
    ProductItemWebRequest productItemRequest = new ProductItemWebRequest();
    productItemRequest.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemRequest.setImages(imageRequests);
    productItemRequest.setAttributesMap(attributesMap);
    productItemRequest.setContentChanged(true);
    ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest = new ProductItemAttributeValueWebRequest();
    productItemAttributeValueWebRequest.setAttribute(attributeWebRequest);
    productItemAttributeValueWebRequest.setValue(ATTR_VALUE);
    productItemAttributeValueWebRequest.setId("ID");
    List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests = new ArrayList<>();
    productItemAttributeValueWebRequests.add(productItemAttributeValueWebRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueWebRequests);
    productItems.add(productItemRequest);
    productWebRequest.setProductItems(productItems);
    productWebRequest.setPostLive(true);
    productWebRequest.setForceReviewNotes(NOTES);

    productWebRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);

    updateBrandWebRequest = new UpdateBrandWebRequest();
    updateBrandWebRequest.setBrandCode(BRAND_CODE);
    updateBrandWebRequest.setBrandDescription(BRAND_DESCRIPTION);
    updateBrandWebRequest.setBrandName(BRAND_NAME);

    File file = new File(Thread.currentThread().getContextClassLoader()
            .getResource("VendorUpdateTestRequest.json").getFile());
    ObjectMapper objectMapper = new ObjectMapper();
    productWebRequestForVendorUpdate = objectMapper.readValue(file, ProductWebRequest.class);

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUserType()).thenReturn(Constants.USER_TYPE_EXTERNAL);
    ReflectionTestUtils.setField(productController, "productNameTrim", true);
  }


  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  public void toProductRequestTest() {
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequest, clientParameterHelper, false, false);
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getBusinessPartnerCode();
    verify(clientParameterHelper).set(any(), any());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(Constants.STORE_ID,
        response.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0)
            .getStoreId());
    assertEquals(GENERATED_ITEM_NAME, response.getProductItems().get(0).getGeneratedItemName());
    assertEquals(CATEGORY_CODE, response.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(CATALOG_CODE, response.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertTrue(response.getProductCategories().get(0).isMarkForDelete());
    assertTrue(response.isPostLive());
    assertEquals(NOTES, response.getForceReviewNotes());
    assertTrue(response.getProductItems().get(0).isContentChanged());
  }

  @Test
  public void toProductRequestNameEditTrimSwitchOnTest() {
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequest, clientParameterHelper, true, false);
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getBusinessPartnerCode();
    verify(clientParameterHelper).set(any(), any());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(Constants.STORE_ID,
        response.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0)
            .getStoreId());
    assertEquals(GENERATED_ITEM_NAME, response.getProductItems().get(0).getGeneratedItemName());
    assertEquals(CATEGORY_CODE, response.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(CATALOG_CODE, response.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertTrue(response.getProductCategories().get(0).isMarkForDelete());
    assertTrue(response.isPostLive());
    assertEquals(NOTES, response.getForceReviewNotes());
    assertTrue(response.getProductItems().get(0).isContentChanged());
  }

  @Test
  public void UpdateBrandRequestTest() {
    UpdateBrandRequest updateBrandRequest = ConverterUtil.toUpdateBrandRequest(updateBrandWebRequest);
    Assertions.assertNotNull(updateBrandRequest);
    assertEquals(BRAND_CODE, updateBrandRequest.getBrandCode());
    assertEquals(BRAND_NAME, updateBrandRequest.getBrandName());
    assertEquals(BRAND_DESCRIPTION, updateBrandRequest.getBrandDescription());
  }

  @Test
  public void convertProductWebRequestToDistributionProductRequestTest() {
    ItemNotesWebRequest itemNotesWebRequest = new ItemNotesWebRequest();
    itemNotesWebRequest.setSkuCode(ITEM_SKU);
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(new NeedRevisionNotesWebRequest());
    productWebRequestForVendorUpdate.getNeedRevisionNotes().setItemNotes(Arrays.asList(itemNotesWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
  }

  @Test
  public void convertProductWebRequestToDistributionProductRequestDifferentKeyTest() {
    ItemNotesWebRequest itemNotesWebRequest = new ItemNotesWebRequest();
    itemNotesWebRequest.setSkuCode("MTA");
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(new NeedRevisionNotesWebRequest());
    productWebRequestForVendorUpdate.getNeedRevisionNotes().setItemNotes(Arrays.asList(itemNotesWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    assertProductImages(distributionProductDetailRequest);
    assertProductAttributes(distributionProductDetailRequest);
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
  }

  @Test
  public void toRejectReasonTest() {
    RejectReason rejectReason = new RejectReason();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReason.setProduct(product);
    RejectReasonRequest rejectReasonOutput = ConverterUtil.toRejectReason(rejectReason);
    Assertions.assertEquals(PRODUCT, rejectReasonOutput.getProduct().get(0));
  }

  @Test
  public void convertProductWebRequestToDistributionProductRequestOriginalImageTest() {
    productWebRequestForVendorUpdate.getImages().get(0).setOriginalImage(true);
    productWebRequestForVendorUpdate.getProductItems().get(0).getImages().get(0).setOriginalImage(true);
    productWebRequestForVendorUpdate.setMarginExceed(true);
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    productWebRequestForVendorUpdate.setProductCategories(Arrays.asList(productCategoryWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    assertTrue(distributionProductDetailRequest.getProductImages().get(0).getOriginalImage());
    assertTrue(distributionProductDetailRequest.getProductItems().get(0).getProductItemImages().get(0).getOriginalImage());
    assertTrue(distributionProductDetailRequest.isMarginExceeded());
    assertEquals(CATEGORY_CODE, distributionProductDetailRequest.getCategoryCode());
  }

  @Test
  public void convertProductWebRequestToDistributionProductRequestWithProductNotesTest() {
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(
        NeedRevisionNotesWebRequest.builder().vendorNotes(Arrays.asList("notes")).allVariants(true)
            .contentAdditionalNotes("content notes").imagesAdditionalNotes("image notes")
            .vendorErrorFields(Arrays.asList("url")).build());
    DistributionProductDetailRequest distributionProductDetailRequest =
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
    Assertions.assertTrue(distributionProductDetailRequest.getProductNotes().getAllVariants());
  }

  @Test
  public void convertProductWebRequestToDistributionProductRequestWithProductItemNotesTest() {
    productWebRequestForVendorUpdate.setNotes(NOTES);
    ItemNotesWebRequest itemNotesWebRequest =
        ItemNotesWebRequest.builder().skuCode(ITEM_SKU).itemName(GENERATED_ITEM_NAME).itemSku(ITEM_SKU).itemNumber(1)
            .vendorNotes(Arrays.asList("image")).vendorErrorFields(Arrays.asList("image")).build();
    productWebRequestForVendorUpdate.setNeedRevisionNotes(
        NeedRevisionNotesWebRequest.builder().vendorNotes(Arrays.asList("notes")).allVariants(true)
            .contentAdditionalNotes("content notes").imagesAdditionalNotes("image notes")
            .vendorErrorFields(Arrays.asList("url")).itemNotes(Arrays.asList(itemNotesWebRequest)).build());
    DistributionProductDetailRequest distributionProductDetailRequest =
        ConverterUtil.convertProductWebRequestToDistributionProductRequest(productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
    Assertions.assertEquals(1,
        distributionProductDetailRequest.getProductItems().get(0).getItemNotes().getVendorNotes().size());
    Assertions.assertEquals(ITEM_SKU,
        distributionProductDetailRequest.getProductItems().get(0).getItemNotes().getSkuCode());
  }

  private void assertProductItems(DistributionProductDetailRequest distributionProductDetailRequest) {
    assertNotNull(distributionProductDetailRequest.getProductItems());
    assertTrue(distributionProductDetailRequest.getProductItems().size() == 1);
    final DistributionProductItemRequest distributionProductItemRequest =
        distributionProductDetailRequest.getProductItems().get(0);
    assertEquals(GENERATED_ITEM_NAME, distributionProductItemRequest.getGeneratedItemName());
    assertEquals(ITEM_SKU, distributionProductItemRequest.getSkuCode());
    assertTrue(0 == distributionProductItemRequest.getDangerousGoodsLevel());
    final DistributionProductAttributeRequest productItemAttribute =
        distributionProductItemRequest.getProductItemAttributes().get(0);
    assertEquals(ATTRIBUTE_CODE, productItemAttribute.getAttributeCode());
    assertEquals(ATTR_VALUE, productItemAttribute.getValue());
    final DistributionProductImageRequest imageRequest =
        distributionProductItemRequest.getProductItemImages().get(0);
    assertEquals(IMAGE_LOCATION_PATH, imageRequest.getLocationPath());
    assertTrue(imageRequest.isEdited());
  }

  private void assertProductAttributes(DistributionProductDetailRequest distributionProductDetailRequest) {
    assertTrue(distributionProductDetailRequest.getProductAttributes().size() == 23);
    final DistributionProductAttributeRequest distributionProductAttributeRequest =
        distributionProductDetailRequest.getProductAttributes().get(0);
    System.out.println(distributionProductAttributeRequest);
    assertEquals(ATTRIBUTE_CODE, distributionProductAttributeRequest.getAttributeCode());
    assertEquals(ATTRIBUTE_NAME, distributionProductAttributeRequest.getName());
    assertEquals(ATTR_VALUE, distributionProductAttributeRequest.getValue());
  }

  private void assertProductImages(DistributionProductDetailRequest distributionProductDetailRequest) {
    assertTrue(distributionProductDetailRequest.getProductImages().size() == 1);
    final DistributionProductImageRequest imageRequest =
        distributionProductDetailRequest.getProductImages().get(0);
    assertEquals(imageRequest.getLocationPath(), IMAGE_LOCATION_PATH);
    assertTrue(imageRequest.getSequence() == 0);
    assertTrue(imageRequest.isMainImage());
    assertFalse(imageRequest.isEdited());
  }

  @Test
  public void toUpdateBrandRequestTest(){
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, -10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
      BrandAuthCreateWebRequest.builder().brandCode(BRAND_CODE).brandName(BRAND_NAME)
        .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
        .authorisationStatus(ACTIVE).build();
    BrandAuthCreateRequest authCreateRequest =
      ConverterUtil.toUpdateBrandRequest(brandAuthCreateWebRequest);
    assertEquals(authCreateRequest.getSellerCode(),SELLER_CODE);
    assertEquals(authCreateRequest.getAuthStartDate(),startDate);
    assertEquals(authCreateRequest.getAuthExpireDate(),endDate);
  }

  @Test
  public void toBrandAuthUpdateRequestTest(){
    Date startDate = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DATE, 1);
    startDate = calendar.getTime();
    calendar.add(Calendar.DATE, -10);
    Date endDate = calendar.getTime();
    brandAuthCreateWebRequest =
        BrandAuthCreateWebRequest.builder().brandCode(BRAND_CODE).brandName(BRAND_NAME)
            .sellerCode(SELLER_CODE).authStartDate(startDate).authExpireDate(endDate)
            .authorisationStatus(ACTIVE).build();
    BrandAuthUpdateRequest authUpdateRequest =
        ConverterUtil.toBrandAuthUpdateRequest(brandAuthCreateWebRequest);
    assertEquals(authUpdateRequest.getSellerCode(),SELLER_CODE);
    assertEquals(authUpdateRequest.getAuthStartDate(),startDate);
    assertEquals(authUpdateRequest.getAuthExpireDate(),endDate);
  }

  @Test
  public void toBrandAuthHistoryRequestTest() {
    BrandAuthHistoryWebRequest brandAuthHistoryWebRequest = new BrandAuthHistoryWebRequest();
    brandAuthHistoryWebRequest.setBrandCode(BRAND_CODE);
    brandAuthHistoryWebRequest.setSellerCode(SELLER_CODE);
    BrandAuthHistoryRequest brandAuthHistoryRequest =
      ConverterUtil.toBrandAuthHistoryRequest(brandAuthHistoryWebRequest);
    assertEquals(brandAuthHistoryRequest.getSellerCode(), SELLER_CODE);
    assertEquals(brandAuthHistoryRequest.getBrandCode(), BRAND_CODE);
  }

  @Test
  public void toCategoryHistoryWebResponseTest() {
    CategoryHistoryResponse categoryHistoryResponse = new CategoryHistoryResponse();
    categoryHistoryResponse.setCategoryCode(CATEGORY_CODE);
    CategoryHistoryWebResponse categoryHistoryWebResponse =
        ConverterUtil.toCategoryHistoryWebResponse(categoryHistoryResponse);
    assertEquals(categoryHistoryWebResponse.getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  void toUploadImageRequestTest() {
    UploadImageRequest uploadImageRequest =
        ConverterUtil.toUploadImageRequest(IMAGE_FILE_NAME, PRODUCT_CODE, new byte[2], true, true,
            ORIGINAL_FILE_NAME);
    assertEquals(IMAGE_FILE_NAME, uploadImageRequest.getImageFileName());
    assertEquals(ORIGINAL_FILE_NAME, uploadImageRequest.getOriginalFileType());
    assertTrue(uploadImageRequest.isRetryRequest());
    assertTrue(uploadImageRequest.isActive());
  }

  @Test
  public void toProductRequestWebpEnabledTest() {
    productWebRequest.getProductItems().getFirst().getImages().getFirst()
      .setType(Constants.NEW_IMAGE_TYPE);
    productWebRequest.getProductItems().getFirst().getImages().getFirst()
      .setLocationPath("image.jpeg");
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequest, clientParameterHelper, false, true);
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getBusinessPartnerCode();
    verify(clientParameterHelper).set(any(), any());
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(Constants.STORE_ID,
      response.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues()
        .get(0).getStoreId());
    assertEquals(GENERATED_ITEM_NAME, response.getProductItems().get(0).getGeneratedItemName());
    assertEquals(CATEGORY_CODE,
      response.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals(CATALOG_CODE,
      response.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertTrue(response.getProductCategories().get(0).isMarkForDelete());
    assertTrue(response.isPostLive());
    assertEquals(NOTES, response.getForceReviewNotes());
    assertTrue(response.getProductItems().get(0).isContentChanged());
    assertEquals("image.webp",
      response.getProductItems().getFirst().getImages().getFirst().getLocationPath());
    assertEquals("image.webp", response.getImages().getFirst().getLocationPath());
  }
}
