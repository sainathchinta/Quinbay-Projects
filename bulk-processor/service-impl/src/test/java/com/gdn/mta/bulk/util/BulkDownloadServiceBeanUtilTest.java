package com.gdn.mta.bulk.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.StoreCopyFailedProducts;
import com.gdn.mta.bulk.models.UpdateSalesCategoryFailedProduct;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicInfoResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;

public class BulkDownloadServiceBeanUtilTest {

  private static final String PRODUCT_SKU = "PRODUCT-01-01";
  private static final String PRODUCT_CODE = "PRODUCT-CODE";
  private static final String PRODUCT_NAME = "Parent Produk 1";
  private static final String ITEM_SKU = "ITEM-01";
  private static final String ITEM_NAME = "Produk 1";
  private static final String SKU_CODE = "SKU-01";
  private static final Integer STOCK = 10;
  private static final Double PRICE = 1000.0;
  private static final Integer PRODUCT_TYPE = 1;
  private static final String PICKUP_POINT_CODE = "PP-00112";
  private static final String MERCHANT_SKU = "BLIBLI";
  private static final String EMAIL_TO = "emailTo";
  private static final String EMAIL_CC = "emailCc";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String FAULTY_IMAGE_TYPE = "faultyImageType";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String RECAT_REQUEST_CODE = "request-code";
  private static final String INTERNAL_REQUEST_CODE = "internal-code";
  public static final String YOUTUBE_REGEX = "(?<=watch\\?v=|/videos/|embed\\/|youtu"
      + ".be\\/|\\/v\\/|\\/e\\/|watch\\?v%3D|watch\\?feature=player_embedded&v=|%2Fvideos%2F"
      + "|embed%\u200C\u200B2F|youtu.be%2F|%2Fv%2F|shorts\\/)[^#\\&\\?\\n]*";

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  private FilterSummaryRequest filterSummaryRequest =
      FilterSummaryRequest.builder().timeFilterType(TimeFilterType.ALL).assigneeEmailId(USERNAME)
          .assignment(Boolean.TRUE).brandPending(Boolean.TRUE)
          .businessPartnerCode(BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
          .contentPending(Boolean.TRUE).faultyImageType(FAULTY_IMAGE_TYPE)
          .imagePending(Boolean.TRUE).postLive(true).vendorCode(VENDOR_CODE).build();

  @InjectMocks
  private BulkDownloadServiceBeanUtil util;

  private List<ProductLevel3SummaryResponse> generateProductLevel3Summaries() {
    List<ProductLevel3SummaryResponse> result = new ArrayList<>();

    List<ProductLevel3PriceResponse> productPrices = new ArrayList<>();
    ProductLevel3PriceResponse productPrice = new ProductLevel3PriceResponse();
    productPrice.setPrice(PRICE);
    productPrice.setSalePrice(PRICE);
    productPrices.add(productPrice);

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    ProductLevel3ViewConfigResponse viewConfig = new ProductLevel3ViewConfigResponse();
    viewConfig.setChannelId(Constant.DEFAULT_CHANNEL);
    viewConfig.setDisplay(true);
    viewConfig.setBuyable(true);
    viewConfigs.add(viewConfig);

    ProductLevel3SummaryResponse product1 = new ProductLevel3SummaryResponse();
    product1.setProductSku(PRODUCT_SKU);
    product1.setProductName(PRODUCT_NAME);
    product1.setItemSku(ITEM_SKU);
    product1.setItemName(ITEM_NAME);
    product1.setSkuCode(SKU_CODE);
    product1.setMerchantSku(MERCHANT_SKU);
    product1.setAvailableStockLevel2(STOCK);
    product1.setProductType(PRODUCT_TYPE);
    product1.setPickupPointCode(PICKUP_POINT_CODE);
    product1.setOff2OnActiveFlag(true);

    product1.setPrices(productPrices);
    product1.setViewConfigs(viewConfigs);
    product1.setMinimumStockLevel2(20);

    result.add(product1);
    return result;
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  private Map<String, Boolean> generatePrivilegeMap() {
    Map<String, Boolean> privilegeMap = new HashMap<>();
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_PRICE, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_STOCK, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_PRODUCT_TYPE, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_O2O, true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    return privilegeMap;
  }

  @Test
  public void testGetAllProductValues_AllPrivileged_O2OTrue_BlibliFulfillmentTrue()
    throws Exception {
    util.getAllProductValues(generatePrivilegeMap(), true, generateProductLevel3Summaries(), true,
      new ProfileResponse(), new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_PickupPoint() throws Exception {
    Map<String, Boolean> priviledgeMap = generatePrivilegeMap();
    priviledgeMap.put(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(priviledgeMap, true, generateProductLevel3Summaries(), true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(PRODUCT_SKU, allProductValues.get(0).get(0));
  }

  @Test
  public void testGetAllProductValues_ForAmphiUser() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(generatePrivilegeMap(), true, generateProductLevel3Summaries(), true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
    Assertions.assertEquals(PRODUCT_SKU, allProductValues.get(0).get(0));
    Assertions.assertEquals(PRODUCT_NAME, allProductValues.get(0).get(1));
    Assertions.assertEquals(ITEM_SKU, allProductValues.get(0).get(2));
    Assertions.assertEquals(PICKUP_POINT_CODE, allProductValues.get(0).get(3));
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_offlineProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_B2BProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_B2BProductTrue() throws Exception {
    ReflectionTestUtils.setField(util, "preOrderQuotaFeatureSwitch", true);
    ProfileResponse profileResponse = ProfileResponse.builder()
        .company(CompanyDTO.builder().salesChannel(List.of(Constant.B2B_SELLER_CHANNEL)).build())
        .flags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE)).build();
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setDisplay(false);
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setBuyable(true);
    productLevel3SummaryResponseList.getFirst().setPreOrder(true);
    productLevel3SummaryResponseList.getFirst().setPreOrderDate(DateUtils.addDays(new Date(), 2));
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(18, allProductValues.getFirst().size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_B2BProductTruePriceNull() throws Exception {
    ReflectionTestUtils.setField(util, "preOrderQuotaFeatureSwitch", true);
    ProfileResponse profileResponse = ProfileResponse.builder()
        .company(CompanyDTO.builder().salesChannel(List.of(Constant.B2B_SELLER_CHANNEL)).build())
        .flags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE)).build();
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setDisplay(false);
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setBuyable(true);
    productLevel3SummaryResponseList.getFirst().setB2BResponse(new B2BResponse());
    productLevel3SummaryResponseList.getFirst().setPreOrder(true);
    productLevel3SummaryResponseList.getFirst().setPreOrderDate(null);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(18, allProductValues.getFirst().size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_B2BProductTruePriceNonNull() throws Exception {
    ReflectionTestUtils.setField(util, "preOrderQuotaFeatureSwitch", true);
    ProfileResponse profileResponse = ProfileResponse.builder()
        .company(CompanyDTO.builder().salesChannel(List.of(Constant.B2B_SELLER_CHANNEL)).build())
        .flags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE)).build();
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setDisplay(false);
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setBuyable(true);
    productLevel3SummaryResponseList.getFirst().setB2BResponse(B2BResponse.builder().basePrice(PRICE).build());
    productLevel3SummaryResponseList.getFirst().setPreOrder(false);
    productLevel3SummaryResponseList.getFirst().setPreOrderDate(DateUtils.addDays(new Date(), 1));
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(18, allProductValues.getFirst().size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_B2BProductTruePriceNonNullManaged() throws Exception {
    ReflectionTestUtils.setField(util, "preOrderQuotaFeatureSwitch", false);
    ProfileResponse profileResponse = ProfileResponse.builder()
        .company(CompanyDTO.builder().salesChannel(Arrays.asList(Constant.B2B_SELLER_CHANNEL)).build())
        .flags(Map.of(ProfileFlagNames.BLIBLI_OMG, Boolean.TRUE)).build();
    ProductLevel3ViewConfigResponse viewConfigResponse = new ProductLevel3ViewConfigResponse();
    viewConfigResponse.setChannelId(Constant.B2B_CHANNEL);
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setDisplay(false);
    productLevel3SummaryResponseList.getFirst().getViewConfigs().getFirst().setBuyable(true);
    productLevel3SummaryResponseList.getFirst().getViewConfigs().add(viewConfigResponse);
    productLevel3SummaryResponseList.getFirst()
        .setB2BResponse(B2BResponse.builder().basePrice(PRICE).managed(true).build());
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(17, allProductValues.getFirst().size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_teaserProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(true);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_ForPureExternalUser_onlineProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_offline() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_teaser() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_PRICE);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(13, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_privilege_read_false() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_PRICE);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_O2O);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, false, productLevel3SummaryResponseList, true,
            new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(12, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_B2B() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_offlineProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_onlineProduct() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_invalidInputProductDisplay() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(null);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_invalidInputViewConfigs() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(null);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_AllPrivileged_O2OTrue_BlibliFulfillmentFalse()
    throws Exception {
    util.getAllProductValues(generatePrivilegeMap(), true, generateProductLevel3Summaries(), false,
      new ProfileResponse(), new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_NotPrivilegedWarehouseStock_O2OTrue_BlibliFulfillmentFalse()
    throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, generateProductLevel3Summaries(), false,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_ForReadStockFalse() throws Exception {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).setSynchronizeStock(true);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_STOCK, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
          new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_ForReadStockReadWareHouseStockPrivilageFalse() throws Exception {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).setSynchronizeStock(true);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_STOCK, true);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            new ProfileResponse(), new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValues_NotPrivilegedWarehouseStock_O2OTrue_BlibliFulfillmentTrue()
    throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK);
    util.getAllProductValues(privilegeMap, true, generateProductLevel3Summaries(), true,
      new ProfileResponse(), new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_EmptyProducts() throws Exception {
    List<ProductLevel3SummaryResponse> products = new ArrayList<>();
    products.add(null);
    util.getAllProductValues(generatePrivilegeMap(), true, products, true, new ProfileResponse(),
        new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_AvailableStockLevel1() throws Exception {
    List<ProductLevel3SummaryResponse> products = generateProductLevel3Summaries();
    products.get(0).setAvailableStockLevel1(STOCK);
    util.getAllProductValues(generatePrivilegeMap(), true, products, true, new ProfileResponse(),
        new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_NotIsPrivilegedToReadAvailableStock() throws Exception {
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_STOCK);
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_STOCK, false);
    util.getAllProductValues(privilegeMap, true, generateProductLevel3Summaries(), true,
      new ProfileResponse(), new ArrayList<>());
  }

  @Test
  public void testGetAllProductValues_getStringForBooleanFalse() throws Exception {
    List<ProductLevel3SummaryResponse> products = generateProductLevel3Summaries();
    products.get(0).getViewConfigs().get(0).setDisplay(false);
    products.get(0).getViewConfigs().get(0).setBuyable(false);
    util.getAllProductValues(generatePrivilegeMap(), true, products, true, new ProfileResponse(),
        new ArrayList<>());
  }

  @Test
  public void testGenerateValidationForWorkbook_AllPriveleged() throws Exception {
    Workbook workbook =
        new XSSFWorkbook(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"));
    util.generateValidationForWorkbook(workbook, 1, 3, true, Constant.PICKUP_POINT_SHEET);
  }

  @Test
  public void testGenerateValidationForWorkbook_NotPrivilegedEditPickupPoint() throws Exception {
    Workbook workbook =
        new XSSFWorkbook(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"));
    util.generateValidationForWorkbook(workbook, 1, 3, false, Constant.PICKUP_POINT_SHEET);
  }
  
  @Test
  public void testGenerateValidationForWorkbook_NotPrivilegedEditProductType() throws Exception {
    Workbook workbook =
        new XSSFWorkbook(Thread.currentThread().getContextClassLoader()
            .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx"));
    util.generateValidationForWorkbook(workbook, 1, 3, true, Constant.PICKUP_POINT_SHEET);
  }

  @Test
  public void getVendorBulkDownloadRequestTest() {
    BulkDownloadRequest vendorSummaryDownloadRequest = util
        .getVendorBulkDownloadRequest(EMAIL_TO, EMAIL_CC, filterSummaryRequest, REQUEST_ID,
            USERNAME);
    Assertions.assertTrue(
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).isUnrestrictedDownload());
    Assertions.assertTrue(((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getAssignment());
    Assertions.assertTrue(
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getBrandPending());
    Assertions.assertTrue(
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getContentPending());
    Assertions.assertEquals(TimeFilterType.ALL.name(),
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getTimeFilterType());
    Assertions.assertEquals(CATEGORY_CODE,
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getCategoryCode());
    Assertions.assertEquals(FAULTY_IMAGE_TYPE,
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getFaultyImageType());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        ((VendorSummaryDownloadRequest) vendorSummaryDownloadRequest).getBusinessPartnerCode());
  }

  @Test
  public void getRecatFailedProductsDownloadRequestTest() {
    BulkDownloadRequest bulkDownloadRequest = util
        .getRecatFailedProductsDownloadRequest(RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Assertions.assertEquals(bulkDownloadRequest.getBulkProcessEntity(), BulkProcessEntity.RECAT_FAILED_PRODUCTS);
  }

  @Test
  public void getInternalProcessFailedDownloadRequestTest() {
    BulkDownloadRequest bulkDownloadRequest =
        util.getInternalProcessFailedDownloadRequest(INTERNAL_REQUEST_CODE, USERNAME, REQUEST_ID,
            BulkInternalProcessType.STORE_COPY.name());
    Assertions.assertEquals(bulkDownloadRequest.getBulkProcessEntity(), BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
  }

  @Test
  public void getInternalProcessFailedDownloadRequestForSalesCategoryTest() {
    BulkDownloadRequest bulkDownloadRequest =
        util.getInternalProcessFailedDownloadRequest(INTERNAL_REQUEST_CODE, USERNAME, REQUEST_ID,
            BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    Assertions.assertEquals(bulkDownloadRequest.getBulkProcessEntity(), BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
  }

  @Test
  public void getInternalProcessFailedDownloadRequestProcessTypeEmptyTest() {
    BulkDownloadRequest bulkDownloadRequest =
        util.getInternalProcessFailedDownloadRequest(INTERNAL_REQUEST_CODE, USERNAME, REQUEST_ID, StringUtils.EMPTY);
    Assertions.assertTrue(Objects.isNull(bulkDownloadRequest.getBulkProcessEntity()));
  }

  @Test
  public void getTargetSellerTemplateForCopyStore() {
    BulkDownloadRequest bulkDownloadRequest = util
        .getTargetSellerTemplateForCopyStore(BUSINESS_PARTNER_CODE, USERNAME, REQUEST_ID);
    Assertions.assertEquals(bulkDownloadRequest.getBulkProcessEntity(), BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE + ".xlsx", bulkDownloadRequest.getFilename());
  }

  @Test
  public void getAllProductsDetailForStoreCopyTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
    Assertions.assertEquals(storeCopy.get(0).get(14), BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
  }

  @Test
  public void getAllProductsDetailForStoreCopyPT2Test() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setProductType(2);
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
    Assertions.assertEquals(storeCopy.get(0).get(14), BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getdescription());
  }

  @Test
  public void getAllProductsDetailForStoreCopyPT3Test() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setProductType(3);
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
    Assertions.assertEquals(storeCopy.get(0).get(14), BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
  }

  @Test
  public void getAllProductsDetailForStoreCopyPTInvalidTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setProductType(4);
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
    Assertions.assertEquals(storeCopy.get(0).get(14), StringUtils.EMPTY);
  }

  @Test
  public void getAllProductsDetailForStoreCopyMinStockNullTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setMinimumStockLevel2(null);
    List<List<String>> storeCopy =
        util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
    Assertions.assertEquals(String.valueOf(0), storeCopy.get(0).get(12));
  }

  @Test
  public void getAllProductsDetailForStoreCopySyncStockTrueTest() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setSynchronizeStock(true);
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
  }

  @Test
  public void generateStoreCopyRowsTest() {
    StoreCopyFailedProducts storeCopyFailedProducts =
        StoreCopyFailedProducts.builder().productCode(PRODUCT_CODE).productName(PRODUCT_NAME).build();
    InternalProcessFailedProductResponse internalProcessFailedProductResponse =
        new InternalProcessFailedProductResponse();
    internalProcessFailedProductResponse.setInternalBulkProcessFailedData(Arrays.asList(storeCopyFailedProducts));
    List<List<String>> row = new ArrayList<>();
    util.generateStoreCopyRows(internalProcessFailedProductResponse, row);
    Assertions.assertEquals(16, row.get(0).size());
  }

  @Test
  public void generateUpdateSalesCategoryRowsTest() {
    UpdateSalesCategoryFailedProduct updateSalesCategoryFailedProduct =
        UpdateSalesCategoryFailedProduct.builder().build();
    InternalProcessFailedProductResponse internalProcessFailedProductResponse =
        new InternalProcessFailedProductResponse();
    internalProcessFailedProductResponse.setInternalBulkProcessFailedData(Arrays.asList(updateSalesCategoryFailedProduct));
    List<List<String>> row = new ArrayList<>();
    util.generateUpdateSalesCategoryRows(internalProcessFailedProductResponse, row);
    Assertions.assertEquals(4, row.get(0).size());
  }

  @Test
  public void getAllProductsDetailForStoreCopyTest_False() {
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = generateProductLevel3Summaries();
    productLevel3SummaryResponses.get(0).setSynchronizeStock(Boolean.TRUE);
    List<List<String>> storeCopy = util.getAllProductsDetailForStoreCopy(productLevel3SummaryResponses);
  }

  @Test
  public void testGetAllProductValues_B2BMppEnabled() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
      util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
        profileResponse, new ArrayList<>());
    Assertions.assertEquals(16, allProductValues.get(0).size());
  }

  @Test
  public void testGetAllProductValuesCnc1P_B2BMppEnabled() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(17, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PPureExternalUser() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(16, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PAmphiUserCnCOff() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(14, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PAmphiUserCnCOffNoAccessibility() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(13, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PAmphiUserTest1() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(17, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PAmphiUserTest2() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(17, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PAmphiUserTest3() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(false);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true, profileResponse, new ArrayList<>());
    Assertions.assertEquals(17, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PCNCSwitchOffNoAccessibility() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(true).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.put(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PCNCSwitchOffCNCOff() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PCNCSwitchOnCNCOff() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(15, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductValuesCnc1PCNCSwitchOnCNCOff1() throws Exception {
    ReflectionTestUtils.setField(util, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().cncActivated(false).build());
    Map<String, Boolean> privilegeMap = generatePrivilegeMap();
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE);
    privilegeMap.remove(BulkParameters.PRIVILEGE_READ_O2O);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponseList = generateProductLevel3Summaries();
    productLevel3SummaryResponseList.get(0).getViewConfigs()
        .add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, true));
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setDisplay(false);
    productLevel3SummaryResponseList.get(0).getViewConfigs().get(0).setBuyable(true);
    privilegeMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    List<List<String>> allProductValues =
        util.getAllProductValues(privilegeMap, true, productLevel3SummaryResponseList, true,
            profileResponse, new ArrayList<>());
    Assertions.assertEquals(13, allProductValues.get(0).size());
    ReflectionTestUtils.setField(util, "cncForWarehouseFeatureSwitch", false);
  }

  @Test
  public void testGetAllProductBasicInfoValues() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    Field instoreField = ProductBasicInfoResponse.class.getDeclaredField("instore");
    instoreField.setAccessible(true);
    instoreField.set(product1, 1); // Set as String
    Field categoryCodeField = ProductBasicInfoResponse.class.getDeclaredField("categoryCode");
    categoryCodeField.setAccessible(true);
    categoryCodeField.set(product1, "CAT001");
    Field logisticAdjustmentField = ProductBasicInfoResponse.class.getDeclaredField("logisticAdjustment");
    logisticAdjustmentField.setAccessible(true);
    logisticAdjustmentField.set(product1, 5);
    response.setProductBasicInfoResponseList(Collections.singletonList(product1));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("CAT001", 10);
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertEquals("1", productData.get(2)); // INSTORE
    assertEquals("Category Hierarchy 1", productData.get(4)); // CATEGORY_CODE
    assertEquals("10", productData.get(22)); // LOGISTIC_ADJUSTMENT
  }

  @Test
  public void testGetAllProductBasicInfoValues_EmptyProductList() throws IllegalAccessException {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    response.setProductBasicInfoResponseList(Collections.emptyList());
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testGetAllProductBasicInfoValues_IsBusinessPartnerO2OFalse() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    Field instoreField = ProductBasicInfoResponse.class.getDeclaredField("instore");
    instoreField.setAccessible(true);
    instoreField.set(product1, 1);
    response.setProductBasicInfoResponseList(Collections.singletonList(product1));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    List<List<String>> result = util.getAllProductBasicInfoValues(false, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertFalse(productData.contains("true")); // INSTORE should be skipped
  }

  @Test
  public void testGetAllProductBasicInfoValues_MissingCategoryCodeInMap() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    Field categoryCodeField = ProductBasicInfoResponse.class.getDeclaredField("categoryCode");
    categoryCodeField.setAccessible(true);
    categoryCodeField.set(product1, "CAT002"); // Not present in the map
    response.setProductBasicInfoResponseList(Collections.singletonList(product1));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertEquals("", productData.get(4)); // CATEGORY_CODE should be empty
  }

  @Test
  public void testGetAllProductBasicInfoValues_MissingLogisticAdjustmentInMap() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    Field categoryCodeField = ProductBasicInfoResponse.class.getDeclaredField("categoryCode");
    categoryCodeField.setAccessible(true);
    categoryCodeField.set(product1, "CAT003"); // Not present in the map
    response.setProductBasicInfoResponseList(Collections.singletonList(product1));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertEquals("0", productData.get(22)); // LOGISTIC_ADJUSTMENT should default to 0
  }

  @Test
  public void testGetAllProductBasicInfoValues_NullFieldValue() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    Field descriptionField = ProductBasicInfoResponse.class.getDeclaredField("description");
    descriptionField.setAccessible(true);
    descriptionField.set(product1, null); // Set field to null
    response.setProductBasicInfoResponseList(Collections.singletonList(product1));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertEquals("", productData.get(6)); // Null field should result in an empty string
  }

  @Test
  public void testGetAllProductBasicInfoValues_MultipleProducts() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    ProductBasicInfoResponse product2 = new ProductBasicInfoResponse();
    Field categoryCodeField = ProductBasicInfoResponse.class.getDeclaredField("categoryCode");
    categoryCodeField.setAccessible(true);
    categoryCodeField.set(product1, "CAT001");
    categoryCodeField.set(product2, "CAT002");
    response.setProductBasicInfoResponseList(Arrays.asList(product1, product2));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");
    categoryCodeAndHierarchyMap.put("CAT002", "Category Hierarchy 2");
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("CAT001", 10);
    categoryCodeAndLogisticAdjustmentMap.put("CAT002", 20);
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(2, result.size());
    assertEquals("Category Hierarchy 1", result.get(0).get(4));
    assertEquals("Category Hierarchy 2", result.get(1).get(4));
  }

  @Test
  public void testGetAllProductBasicInfoValues_MultiplePrefixProducts() throws Exception {
    ReflectionTestUtils.setField(util, "youtubeRegex", YOUTUBE_REGEX);
    ReflectionTestUtils.setField(util, "videoStaticBaseUrlPrefix", "true");
    ReflectionTestUtils.setField(util, "imageStaticBaseUrlPrefix", "true");
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product1 = new ProductBasicInfoResponse();
    ProductBasicInfoResponse product2 = new ProductBasicInfoResponse();
    Field videoUrl = ProductBasicInfoResponse.class.getDeclaredField("videoUrl");
    videoUrl.setAccessible(true);
    videoUrl.set(product1, "CAT001");
    response.setProductBasicInfoResponseList(Arrays.asList(product1, product2));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");
    categoryCodeAndHierarchyMap.put("CAT002", "Category Hierarchy 2");
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("CAT001", 10);
    categoryCodeAndLogisticAdjustmentMap.put("CAT002", 20);
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(2, result.size());
  }

  @Test
  public void testProcessFieldForProductData_CategoryCode() throws Exception {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field categoryCodeField = ProductBasicInfoResponse.class.getDeclaredField("categoryCode");
    categoryCodeField.setAccessible(true);
    categoryCodeField.set(product, "CAT001");

    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, categoryCodeAndHierarchyMap, new HashMap<>(), product,
        productData, "CAT001", categoryCodeField, "categoryCode");

    assertEquals("Category Hierarchy 1", productData.get(0));
  }

  @Test
  public void testProcessFieldForProductData_CommonImage() throws Exception {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field commonImageList = ProductBasicInfoResponse.class.getDeclaredField("commonImageList");
    commonImageList.setAccessible(true);
    commonImageList.set(product, new ArrayList<>());

    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("CAT001", 10);

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, new HashMap<>(), categoryCodeAndLogisticAdjustmentMap,
        product, productData, "CAT001", commonImageList, "commonImageList");

    assertEquals(0, productData.size());
  }

  @Test
  public void testProcessFieldForProductData_LogisticAdjustment() throws Exception {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field logisticAdjustmentField = ProductBasicInfoResponse.class.getDeclaredField("logisticAdjustment");
    logisticAdjustmentField.setAccessible(true);
    logisticAdjustmentField.set(product, 5);

    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("CAT001", 10);

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, new HashMap<>(), categoryCodeAndLogisticAdjustmentMap,
        product, productData, "CAT001", logisticAdjustmentField, "logisticAdjustment");

    assertEquals("10", productData.get(0));
  }

  @Test
  public void testProcessFieldForProductData_DefaultCase() throws Exception {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field descriptionField = ProductBasicInfoResponse.class.getDeclaredField("description");
    descriptionField.setAccessible(true);
    descriptionField.set(product, "Sample Description");

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, new HashMap<>(), new HashMap<>(), product, productData,
        "CAT001", descriptionField, "description");

    assertEquals("Sample Description", productData.get(0));
  }

  @Test
  public void testGetAllProductBasicInfoValues_AllFields() throws Exception {
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    ReflectionTestUtils.setField(util, "youtubeRegex", YOUTUBE_REGEX);
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field[] fields = ProductBasicInfoResponse.class.getDeclaredFields();
    for (Field field : fields) {
      field.setAccessible(true);
      if (field.getType().equals(String.class)) {
        field.set(product, "testValue");
      } else if (field.getType().equals(Integer.class)) {
        field.set(product, 1);
      }
    }
    response.setProductBasicInfoResponseList(Collections.singletonList(product));
    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("testValue", "Category Hierarchy");
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();
    categoryCodeAndLogisticAdjustmentMap.put("testValue", 10);
    List<List<String>> result = util.getAllProductBasicInfoValues(true, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertEquals(23, productData.size()); // Ensure all fields are processed
    assertTrue(productData.contains("testValue"));
    assertTrue(productData.contains("10")); // LOGISTIC_ADJUSTMENT
    assertTrue(productData.contains("Category Hierarchy")); // CATEGORY_CODE
  }

  @Test
  public void testProcessFieldForProductData_VideoUrlEmpty() throws Exception {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field videoUrlField = ProductBasicInfoResponse.class.getDeclaredField("videoUrl");
    videoUrlField.setAccessible(true);
    videoUrlField.set(product, "");

    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, categoryCodeAndHierarchyMap, new HashMap<>(), product, productData, "CAT001", videoUrlField, "videoUrl");

    assertEquals(StringUtils.EMPTY, productData.get(0));
  }

  @Test
  public void testProcessFieldForProductData_VideoUrlIsYoutube() throws Exception {
    ReflectionTestUtils.setField(util, "youtubeRegex", YOUTUBE_REGEX);
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field videoUrlField = ProductBasicInfoResponse.class.getDeclaredField("videoUrl");
    videoUrlField.setAccessible(true);
    videoUrlField.set(product, "https://www.youtube.com/watch?v=Gh-XKNuvvC4&t=1s");

    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, categoryCodeAndHierarchyMap, new HashMap<>(), product, productData, "CAT001", videoUrlField, "videoUrl");

    assertEquals("https://www.youtube.com/watch?v=Gh-XKNuvvC4&t=1s", productData.get(0));
  }

  @Test
  public void testProcessFieldForProductData_VideoUrlIsVideo() throws Exception {
    ReflectionTestUtils.setField(util, "youtubeRegex", YOUTUBE_REGEX);
    ReflectionTestUtils.setField(util, "videoStaticBaseUrlPrefix", "https://static-uatb.gdn-app.com/");
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Field videoUrlField = ProductBasicInfoResponse.class.getDeclaredField("videoUrl");
    videoUrlField.setAccessible(true);
    videoUrlField.set(product, "videos/final/TOQ-70043/618f7682-cdcb-4ac5-9786-e6dd4a1e78dc/122810-726391893_tiny__1__compressed.mp4");

    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    categoryCodeAndHierarchyMap.put("CAT001", "Category Hierarchy 1");

    List<String> productData = new ArrayList<>();
    util.processFieldForProductData(true, categoryCodeAndHierarchyMap, new HashMap<>(), product, productData, "CAT001", videoUrlField, "videoUrl");

    assertEquals("https://static-uatb.gdn-app.com/videos/final/TOQ-70043/618f7682-cdcb-4ac5-9786-e6dd4a1e78dc/122810-726391893_tiny__1__compressed.mp4", productData.get(0));
  }

  @Test
  public void testGetAllProductBasicInfoValues_IllegalAccessException() throws Exception {
    // Arrange
    BulkDownloadServiceBeanUtil util = new BulkDownloadServiceBeanUtil();
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    response.setProductBasicInfoResponseList(Collections.singletonList(product));

    Map<String, String> categoryCodeAndHierarchyMap = new HashMap<>();
    Map<String, Integer> categoryCodeAndLogisticAdjustmentMap = new HashMap<>();

    // Act
    List<List<String>> result = util.getAllProductBasicInfoValues(false, response, categoryCodeAndHierarchyMap,
        categoryCodeAndLogisticAdjustmentMap);

    // Assert
    assertEquals(1, result.size());
    List<String> productData = result.get(0);
    assertTrue(productData.contains(StringUtils.EMPTY)); // Ensure empty string is added
  }
}
