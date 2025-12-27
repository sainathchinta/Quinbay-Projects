package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.CompanyVO;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

class ObjectConverterUtilTest {

  private static final String TEST_PRISTINE_ID = "TEST-PRISTINE-ID";
  private static final String TEST_ATTRIBUTE_KEY = "color";
  private static final String TEST_ATTRIBUTE_VALUE = "red";
  private static final String TEST_CATALOG_CODE = "TEST-CATALOG";
  private static final String TEST_CATEGORY_CODE = "CAT-001";
  private static final String TEST_CATEGORY_NAME = "Electronics";
  private static final String TEST_STORE_ID = "TEST-STORE";
  private static final String TEST_UPDATED_BY = "test-user";
  private static final String TEST_BYTES_STRING = "Test String";
  private static final String TEST_BUSINESS_PARTNER_CODE = "BP-001";
  private static final String TEST_BUSINESS_PARTNER_NAME = "Test Business Partner";
  private static final String TEST_COMPANY_NAME = "Test Company";
  private static final String TEST_MERCHANT_TYPE = "MERCHANT";
  private static final String TEST_DELIVERY_TYPE = "STANDARD";
  private static final String TEST_INVENTORY_FULFILLMENT = "FULFILLMENT";
  private static final String TEST_BUSINESS_PARTNER_ALIAS = "Test Alias";
  private static final String TEST_ADJUSTMENT_NAME = "TEST-ADJUSTMENT";
  private static final String TEST_CAMPAIGN_CODE = "CAMP-001";
  private static final String TEST_PREORDER_TYPE_DAYS = "DAYS";
  private static final String TEST_PREORDER_TYPE_WEEK = "WEEK";

  @Test
  void testConvertPristineAttributeToProductAttribute_WithValidInput() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(TEST_PRISTINE_ID);
    Map<String, String> attributes = new HashMap<>();
    attributes.put(TEST_ATTRIBUTE_KEY, TEST_ATTRIBUTE_VALUE);
    pristineDataItem.setPristineListingAttributes(attributes);

    ProductAttribute result =
        ObjectConverterUtil.convertPristineAttributeToProductAttribute(pristineDataItem);

    assertNotNull(result);
    assertEquals(TEST_PRISTINE_ID, result.getItemSku());
    assertEquals(1, result.getProductAttributeDetails().size());
    assertEquals(TEST_ATTRIBUTE_KEY,
        result.getProductAttributeDetails().get(0).getAttributeCode());
    assertEquals(TEST_ATTRIBUTE_VALUE,
        result.getProductAttributeDetails().get(0).getAttributeValue());
  }

  @Test
  void testConvertPristineAttributeToProductAttribute_WithNullInput() {
    assertThrows(ApplicationRuntimeException.class,
        () -> ObjectConverterUtil.convertPristineAttributeToProductAttribute(null));
  }

  @Test
  void testConvertPristineAttributeToProductAttribute_WithEmptyAttributes() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(TEST_PRISTINE_ID);
    pristineDataItem.setPristineListingAttributes(new HashMap<>());

    ProductAttribute result =
        ObjectConverterUtil.convertPristineAttributeToProductAttribute(pristineDataItem);

    assertNotNull(result);
    assertEquals(TEST_PRISTINE_ID, result.getItemSku());
    assertTrue(result.getProductAttributeDetails().isEmpty());
  }

  @Test
  void testConvertBytesToString_WithValidBytes() {
    byte[] bytes = TEST_BYTES_STRING.getBytes();

    String result = ObjectConverterUtil.convertBytesToString(bytes);

    assertNotNull(result);
    assertEquals(TEST_BYTES_STRING, result);
  }

  @Test
  void testConvertBytesToString_WithNullBytes() {
    String result = ObjectConverterUtil.convertBytesToString(null);

    assertNull(result);
  }

  @Test
  void testConvertBytesToString_WithEmptyBytes() {
    byte[] bytes = new byte[0];

    String result = ObjectConverterUtil.convertBytesToString(bytes);

    assertNotNull(result);
    assertEquals("", result);
  }

  @Test
  void testConvertToListOfItemCatalog_WithValidInput() {
    CategoryResponse categoryResponse = createCategoryResponse();
    List<CategoryResponse> categoriesList = Arrays.asList(categoryResponse);
    List<List<CategoryResponse>> listOfCategoriesList = Arrays.asList(categoriesList);

    List<ItemCatalogVO> result = ObjectConverterUtil.convertToListOfItemCatalog(listOfCategoriesList);

    assertNotNull(result);
    assertEquals(1, result.size());
    ItemCatalogVO itemCatalog = result.get(0);
    assertEquals(TEST_CATALOG_CODE, itemCatalog.getCatalogId());
    assertEquals(1, itemCatalog.getItemCategories().size());
    assertEquals(TEST_CATEGORY_NAME, itemCatalog.getItemCategories().get(0).getCategory());
  }

  @Test
  void testConvertToListOfItemCatalog_WithNullInput() {
    assertThrows(ApplicationRuntimeException.class,
        () -> ObjectConverterUtil.convertToListOfItemCatalog(null));
  }

  @Test
  void testConvertToListOfItemCatalog_WithEmptyList() {
    List<List<CategoryResponse>> listOfCategoriesList = new ArrayList<>();

    List<ItemCatalogVO> result = ObjectConverterUtil.convertToListOfItemCatalog(listOfCategoriesList);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testConvertToListOfItemCatalog_WithEmptyCategoriesList() {
    List<CategoryResponse> categoriesList = new ArrayList<>();
    List<List<CategoryResponse>> listOfCategoriesList = Arrays.asList(categoriesList);

    List<ItemCatalogVO> result = ObjectConverterUtil.convertToListOfItemCatalog(listOfCategoriesList);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testConvertToListOfItemCatalogV2_WithValidInput() {
    CategoryResponse categoryResponse = createCategoryResponse();
    List<CategoryResponse> categoriesList = Arrays.asList(categoryResponse);
    List<List<CategoryResponse>> listOfCategoriesList = Arrays.asList(categoriesList);

    List<ItemCatalogVOV2> result =
        ObjectConverterUtil.convertToListOfItemCatalogV2(listOfCategoriesList);

    assertNotNull(result);
    assertEquals(1, result.size());
    ItemCatalogVOV2 itemCatalog = result.get(0);
    assertEquals(TEST_CATALOG_CODE, itemCatalog.getCatalogId());
    assertEquals(1, itemCatalog.getItemCategories().size());
    assertEquals(TEST_CATEGORY_NAME, itemCatalog.getItemCategories().get(0).getCategory());
  }

  @Test
  void testConvertToListOfItemCatalogV2_WithNullInput() {
    assertThrows(ApplicationRuntimeException.class,
        () -> ObjectConverterUtil.convertToListOfItemCatalogV2(null));
  }

  @Test
  void testConvertToListOfItemCatalogV2_WithNullCategoriesList() {
    List<List<CategoryResponse>> listOfCategoriesList = new ArrayList<>();
    listOfCategoriesList.add(null);

    List<ItemCatalogVOV2> result =
        ObjectConverterUtil.convertToListOfItemCatalogV2(listOfCategoriesList);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testConvertToListOfItemCatalogV2_WithEmptyList() {
    List<List<CategoryResponse>> listOfCategoriesList = new ArrayList<>();

    List<ItemCatalogVOV2> result =
        ObjectConverterUtil.convertToListOfItemCatalogV2(listOfCategoriesList);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testConvertToListOfItemCatalogV2_WithEmptyCategoriesList() {
    List<CategoryResponse> categoriesList = new ArrayList<>();
    List<List<CategoryResponse>> listOfCategoriesList = Arrays.asList(categoriesList);

    List<ItemCatalogVOV2> result =
        ObjectConverterUtil.convertToListOfItemCatalogV2(listOfCategoriesList);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testGenerateMandatoryRequestParam_WithValidInput() {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setStoreId(TEST_STORE_ID);
    businessPartnerChange.setUpdatedBy(TEST_UPDATED_BY);

    assertDoesNotThrow(() -> {
      MandatoryRequestParam result =
          ObjectConverterUtil.generateMandatoryRequestParam(businessPartnerChange);
      assertNotNull(result);
      assertEquals(TEST_STORE_ID, result.getStoreId());
    });
  }

  @Test
  void testGenerateMandatoryRequestParam_WithNullInput() {
    assertThrows(ApplicationRuntimeException.class,
        () -> ObjectConverterUtil.generateMandatoryRequestParam(null));
  }

  @Test
  void testGenerateMandatoryRequestParam_WithNullStoreId() {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setStoreId(null);
    businessPartnerChange.setUpdatedBy(TEST_UPDATED_BY);

    assertThrows(Exception.class,
        () -> ObjectConverterUtil.generateMandatoryRequestParam(businessPartnerChange));
  }

  @Test
  void testGenerateMandatoryRequestParam_WithNullUpdatedBy() {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setStoreId(TEST_STORE_ID);
    businessPartnerChange.setUpdatedBy(null);

    assertThrows(Exception.class,
        () -> ObjectConverterUtil.generateMandatoryRequestParam(businessPartnerChange));
  }

  @Test
  void testToCategoryList_WithValidCategoryCode() {
    com.gdn.x.product.model.entity.Category result =
        ObjectConverterUtil.toCategoryList(TEST_CATEGORY_CODE);

    assertNotNull(result);
    assertEquals(TEST_CATEGORY_CODE, result.getCategoryCode());
  }

  @Test
  void testToCategoryList_WithNullCategoryCode() {
    com.gdn.x.product.model.entity.Category result =
        ObjectConverterUtil.toCategoryList(null);

    assertNotNull(result);
    assertNull(result.getCategoryCode());
  }

  @Test
  void testToCategoryList_WithEmptyCategoryCode() {
    String emptyCode = "";

    com.gdn.x.product.model.entity.Category result = ObjectConverterUtil.toCategoryList(emptyCode);

    assertNotNull(result);
    assertEquals(emptyCode, result.getCategoryCode());
  }

  @Test
  void testConvertToDiscountPriceModel_WithValidInput() {
    com.gdn.x.product.model.entity.DiscountPrice discountPrice =
        new com.gdn.x.product.model.entity.DiscountPrice();
    discountPrice.setDiscountPrice(100.0);
    discountPrice.setAdjustmentName(TEST_ADJUSTMENT_NAME);
    discountPrice.setAdjustmentType(com.gdn.x.product.model.entity.AdjustmentType.BLIBLI);
    discountPrice.setCampaignCode(TEST_CAMPAIGN_CODE);

    com.gdn.x.product.domain.event.model.DiscountPriceModel result =
        ObjectConverterUtil.convertToDiscountPriceModel(discountPrice);

    assertNotNull(result);
    assertEquals(100.0, result.getDiscountPrice());
    assertEquals(TEST_ADJUSTMENT_NAME, result.getAdjustmentName());
    assertEquals(com.gdn.x.product.model.entity.AdjustmentType.BLIBLI, result.getAdjustmentType());
    assertEquals(TEST_CAMPAIGN_CODE, result.getCampaignCode());
  }

  @Test
  void testConvertToDiscountPriceModel_WithNullInput() {
    assertThrows(NullPointerException.class,
        () -> ObjectConverterUtil.convertToDiscountPriceModel(null));
  }

  @Test
  void testToMerchantPromoDiscountPrice_WithValidInput() {
    com.gdn.x.product.domain.event.model.DiscountPriceModel discountPriceModel =
        new com.gdn.x.product.domain.event.model.DiscountPriceModel();
    discountPriceModel.setDiscountPrice(150.0);
    discountPriceModel.setAdjustmentName(TEST_ADJUSTMENT_NAME);
    discountPriceModel.setCampaignCode(TEST_CAMPAIGN_CODE);

    com.gdn.x.product.model.entity.DiscountPrice result =
        ObjectConverterUtil.toMerchantPromoDiscountPrice(discountPriceModel);

    assertNotNull(result);
    assertEquals(150.0, result.getDiscountPrice());
    assertEquals(TEST_ADJUSTMENT_NAME, result.getAdjustmentName());
    assertEquals(TEST_CAMPAIGN_CODE, result.getCampaignCode());
  }

  @Test
  void testToMerchantPromoDiscountPrice_WithNullInput() {
    assertThrows(NullPointerException.class,
        () -> ObjectConverterUtil.toMerchantPromoDiscountPrice(null));
  }

  @Test
  void testCheckPreOrderDateStatus_WithPastDate() {
    java.util.Date pastDate = new java.util.Date(System.currentTimeMillis() - 86400000L);

    boolean result = ObjectConverterUtil.checkPreOrderDateStatus(pastDate);

    assertTrue(result);
  }

  @Test
  void testCheckPreOrderDateStatus_WithFutureDate() {
    java.util.Date futureDate = new java.util.Date(System.currentTimeMillis() + 86400000L);

    boolean result = ObjectConverterUtil.checkPreOrderDateStatus(futureDate);

    assertTrue(!result);
  }

  @Test
  void testCheckPreOrderDateStatus_WithTodayDate() {
    java.util.Calendar cal = java.util.Calendar.getInstance();
    cal.set(java.util.Calendar.HOUR_OF_DAY, 0);
    cal.set(java.util.Calendar.MINUTE, 0);
    cal.set(java.util.Calendar.SECOND, 0);
    cal.set(java.util.Calendar.MILLISECOND, 0);
    java.util.Date today = cal.getTime();

    boolean result = ObjectConverterUtil.checkPreOrderDateStatus(today);

    assertTrue(result);
  }

  @Test
  void testCheckPreOrderDateStatus_WithNullDate() {
    assertThrows(NullPointerException.class,
        () -> ObjectConverterUtil.checkPreOrderDateStatus(null));
  }

  @Test
  void testCheckPreOrder_WithValidPreOrder() {
    com.gdn.x.product.model.entity.Product product =
        new com.gdn.x.product.model.entity.Product();
    com.gdn.x.product.model.entity.PreOrder preOrder =
        new com.gdn.x.product.model.entity.PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(TEST_PREORDER_TYPE_DAYS);
    preOrder.setPreOrderValue(7);
    product.setPreOrder(preOrder);

    com.gdn.x.product.model.vo.PreOrderVO result = ObjectConverterUtil.checkPreOrder(product);

    assertNotNull(result);
    assertEquals(TEST_PREORDER_TYPE_DAYS, result.getPreOrderType());
    assertEquals(7, result.getPreOrderValue());
  }

  @Test
  void testCheckPreOrder_WithWeekType() {
    com.gdn.x.product.model.entity.Product product =
        new com.gdn.x.product.model.entity.Product();
    com.gdn.x.product.model.entity.PreOrder preOrder =
        new com.gdn.x.product.model.entity.PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(TEST_PREORDER_TYPE_WEEK);
    preOrder.setPreOrderValue(2);
    product.setPreOrder(preOrder);

    com.gdn.x.product.model.vo.PreOrderVO result = ObjectConverterUtil.checkPreOrder(product);

    assertNotNull(result);
    assertEquals(TEST_PREORDER_TYPE_DAYS, result.getPreOrderType());
    assertEquals(14, result.getPreOrderValue());
  }

  @Test
  void testCheckPreOrder_WithNullPreOrder() {
    com.gdn.x.product.model.entity.Product product =
        new com.gdn.x.product.model.entity.Product();

    com.gdn.x.product.model.vo.PreOrderVO result = ObjectConverterUtil.checkPreOrder(product);

    assertNotNull(result);
    assertEquals(false, result.getIsPreOrder());
  }

  @Test
  void testCheckPreOrder_WithPreOrderFalse() {
    com.gdn.x.product.model.entity.Product product =
        new com.gdn.x.product.model.entity.Product();
    com.gdn.x.product.model.entity.PreOrder preOrder =
        new com.gdn.x.product.model.entity.PreOrder();
    preOrder.setIsPreOrder(false);
    product.setPreOrder(preOrder);

    com.gdn.x.product.model.vo.PreOrderVO result = ObjectConverterUtil.checkPreOrder(product);

    assertNotNull(result);
    assertEquals(false, result.getIsPreOrder());
  }

  @Test
  void testConvertToSalesCatalogsFromSalesCatalogsModel_WithValidInput() {
    com.gdn.x.product.domain.event.model.SalesCatalogModel model =
        new com.gdn.x.product.domain.event.model.SalesCatalogModel(TEST_CATALOG_CODE,
            Arrays.asList(TEST_CATEGORY_CODE));
    List<com.gdn.x.product.domain.event.model.SalesCatalogModel> models =
        Arrays.asList(model);

    List<com.gdn.x.product.model.entity.SalesCatalog> result =
        ObjectConverterUtil.convertToSalesCatalogsFromSalesCatalogsModel(models);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(TEST_CATALOG_CODE, result.get(0).getCatalogCode());
    assertEquals(1, result.get(0).getListOfCategories().size());
    assertEquals(TEST_CATEGORY_CODE, result.get(0).getListOfCategories().get(0).getCategoryCode());
  }

  @Test
  void testConvertToSalesCatalogsFromSalesCatalogsModel_WithEmptyList() {
    List<com.gdn.x.product.domain.event.model.SalesCatalogModel> models = new ArrayList<>();

    List<com.gdn.x.product.model.entity.SalesCatalog> result =
        ObjectConverterUtil.convertToSalesCatalogsFromSalesCatalogsModel(models);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testConvertToSalesCatalogsFromSalesCatalogsModel_WithNullInput() {
    assertThrows(NullPointerException.class,
        () -> ObjectConverterUtil.convertToSalesCatalogsFromSalesCatalogsModel(null));
  }

  private CategoryResponse createCategoryResponse() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(TEST_CATEGORY_CODE);
    categoryResponse.setName(TEST_CATEGORY_NAME);
    categoryResponse.setActivated(true);
    categoryResponse.setDisplay(true);
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogCode(TEST_CATALOG_CODE);
    categoryResponse.setCatalog(catalogResponse);
    return categoryResponse;
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithValidInput() {
    BusinessPartnerChange businessPartnerChange = createTestBusinessPartnerChange();
    BusinessPartner existingBusinessPartner = new BusinessPartner();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, existingBusinessPartner);

    assertNotNull(result);
    assertEquals(TEST_BUSINESS_PARTNER_CODE, result.getBusinessPartnerCode());
    assertEquals(TEST_BUSINESS_PARTNER_NAME, result.getBusinessPartnerName());
    assertEquals(TEST_COMPANY_NAME, result.getName());
    assertEquals(TEST_MERCHANT_TYPE, result.getMerchantType());
    assertTrue(result.isInternationalFlag());
    assertTrue(result.isUmkmFlag());
    assertTrue(result.isOfflineToOnlineFlag());
    assertTrue(result.isCncActivated());
    assertTrue(result.isSupplierFlag());
    assertTrue(result.isMerchantFlag());
    assertTrue(result.isCustomerFlag());
    assertEquals(TEST_DELIVERY_TYPE, result.getMerchantDeliveryType());
    assertEquals(TEST_STORE_ID, result.getStoreId());
    assertEquals(TEST_INVENTORY_FULFILLMENT, result.getInventoryFulfillment());
    assertEquals(TEST_BUSINESS_PARTNER_ALIAS, result.getBusinessPartnerAlias());
    assertNotNull(result.getSalesChannel());
    assertTrue(result.getSalesChannel().isEmpty());
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithNullBusinessPartner() {
    BusinessPartnerChange businessPartnerChange = createTestBusinessPartnerChange();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, null);

    assertNotNull(result);
    assertEquals(TEST_BUSINESS_PARTNER_CODE, result.getBusinessPartnerCode());
    assertEquals(TEST_BUSINESS_PARTNER_NAME, result.getBusinessPartnerName());
    assertEquals(TEST_COMPANY_NAME, result.getName());
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithNullCompany() {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setBusinessPartnerCode(TEST_BUSINESS_PARTNER_CODE);
    businessPartnerChange.setStoreId(TEST_STORE_ID);
    BusinessPartner existingBusinessPartner = new BusinessPartner();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, existingBusinessPartner);

    assertNotNull(result);
    assertEquals(existingBusinessPartner, result);
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithSalesChannel() {
    BusinessPartnerChange businessPartnerChange = createTestBusinessPartnerChange();
    List<String> salesChannels = Arrays.asList("ONLINE", "OFFLINE");
    businessPartnerChange.getCompany().setSalesChannel(salesChannels);
    BusinessPartner existingBusinessPartner = new BusinessPartner();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, existingBusinessPartner);

    assertNotNull(result);
    assertEquals(salesChannels, result.getSalesChannel());
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithFlags() {
    BusinessPartnerChange businessPartnerChange = createTestBusinessPartnerChange();
    Map<String, Object> flags = new HashMap<>();
    flags.put(ProfileFlagNames.BLIBLI_OMG, true);
    businessPartnerChange.setFlags(flags);
    BusinessPartner existingBusinessPartner = new BusinessPartner();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, existingBusinessPartner);

    assertNotNull(result);
    assertTrue(result.getSellerOmg());
  }

  @Test
  void testConvertBusinessPartnerChangeToBusinessPartner_WithNullFlags() {
    BusinessPartnerChange businessPartnerChange = createTestBusinessPartnerChange();
    businessPartnerChange.setFlags(null);
    BusinessPartner existingBusinessPartner = new BusinessPartner();

    BusinessPartner result = ObjectConverterUtil.convertBusinessPartnerChangeToBusinessPartner(
        businessPartnerChange, existingBusinessPartner);

    assertNotNull(result);
    assertNull(result.getSellerOmg());
  }

  private BusinessPartnerChange createTestBusinessPartnerChange() {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setBusinessPartnerCode(TEST_BUSINESS_PARTNER_CODE);
    businessPartnerChange.setStoreId(TEST_STORE_ID);
    businessPartnerChange.setAllCategory(true);

    CompanyVO company = new CompanyVO();
    company.setBusinessPartnerName(TEST_BUSINESS_PARTNER_NAME);
    company.setName(TEST_COMPANY_NAME);
    company.setMerchantType(TEST_MERCHANT_TYPE);
    company.setInternationalFlag(true);
    company.setUmkmFlag(true);
    company.setOfflineToOnlineFlag(true);
    company.setCncActivated(true);
    company.setBusinessPartnerAlias(TEST_BUSINESS_PARTNER_ALIAS);
    company.setSupplierFlag(true);
    company.setMerchantFlag(true);
    company.setCustomerFlag(true);
    company.setMerchantDeliveryType(TEST_DELIVERY_TYPE);
    company.setInventoryFulfillment(TEST_INVENTORY_FULFILLMENT);
    company.setSalesChannel(new ArrayList<>());

    businessPartnerChange.setCompany(company);
    return businessPartnerChange;
  }

  @Test
  void getAiGeneratedFields_whenSourceIsNotNull_shouldCopyFields() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse aiGeneratedFieldsResponse = new com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse();
    aiGeneratedFieldsResponse.setAiGeneratedCategory(true);
    productDetailResponse.setAiGeneratedFieldsResponse(aiGeneratedFieldsResponse);

    AiGeneratedFieldsResponse result = ObjectConverterUtil.getAiGeneratedFields(productDetailResponse);
    assertEquals(true, result.isAiGeneratedCategory());
  }

  @Test
  void getAiGeneratedFields_whenSourceIsNull_shouldReturnEmptyObject() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setAiGeneratedFieldsResponse(null);

    AiGeneratedFieldsResponse result =  ObjectConverterUtil.getAiGeneratedFields(productDetailResponse);

    assertFalse(result.isAiGeneratedBrand());
    assertFalse(result.isAiGeneratedCategory());
  }

}

