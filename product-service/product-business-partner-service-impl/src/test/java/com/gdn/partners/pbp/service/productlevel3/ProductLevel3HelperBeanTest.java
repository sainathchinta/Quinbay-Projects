package com.gdn.partners.pbp.service.productlevel3;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import com.gda.mta.product.dto.B2bFieldsRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductL3CommonImageRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.warehouse.WareHouseOutBound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SkuCodeBundleRecipeResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

public class ProductLevel3HelperBeanTest {

  private static final String PRODUCT_CODE = "MTA-0001";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_NAME = "Produk 1";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String ID = "id";
  private static final String CATEGORY_NAME = "catName";
  private static final String VENDOR_NOTES = "vendor notes";
  private static final String BUSINESS_PARTNER_CODE = "bpc";
  private static final String RESTRICTED_KEYWORD = "<";
  private static final String RESTRICTED_KEYWORD_1 = "adult";

  private static final String RESTRICTED_KEYWORD_2 = "restricted";
  private static final String USP_WITH_RESTRICTED_KEYWORD = "adult usp";
  private static final String NAME_WITH_RESTRICTED_KEYWORD = "<name";
  private static final String IMAGE_LOCATION_PATH_1 = "path1";
  private static final String IMAGE_LOCATION_PATH_2 = "path2";
  private static final String IMAGE_LOCATION_PATH_3 = "path3";
  private static final String IMAGE_LOCATION_PATH_4 = "path4";
  private static final String IMAGE_LOCATION_PATH_5 = "path5";
  private static final String IMAGE_HASH_CODE_1 = "hashCode1";
  private static final String IMAGE_HASH_CODE_2 = "hashCode2";
  private static final String IMAGE_HASH_CODE_3 = "hashCode3";
  private static final String IMAGE_HASH_CODE_4 = "hashCode4";
  private static final String IMAGE_HASH_CODE_5 = "hashCode5";
  private static final String USP = "usp";
  private static final String DESCRIPTION = "description";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU_1 = "ABC-10001-10001-00001";
  private static final Double OLD_SALE_PRICE = 1000.0;
  private static final Double NEW_SALE_PRICE = 2000.0;
  private static final Double OLD_OFFER_PRICE = 1000.0;
  private static final Double NEW_OFFER_PRICE = 2000.0;
  private static final String OLD_SELLER_SKU = "oldSellerSku";
  private static final String NEW_SELLER_SKU = "newSellerSku";
  private static final String BRAND_CODE = "brandCode";
  private static final String ITEM_CODE_1 = "itemCode1";
  private static final String ITEM_CODE_2 = "itemCode2";
  private static final String RED = "red";
  private static String FAMILY_COLOUR_ATTRIBUTE_CODE = "FAM-00001";
  private static String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";

  @InjectMocks
  private ProductLevel3HelperBean productLevel3HelperBean;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private WareHouseOutBound wareHouseOutBound;

  @Captor
  private ArgumentCaptor<CreateUpdateBillOfMaterialRecipeCommandRequest> createUpdateBillOfMaterialRecipeRequestArgumentCaptor;

  private ProductCollection productCollection;
  private ProductDetailResponse productDetailResponse;
  private QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
  private ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
  private PriceDTO oldPriceDTO = new PriceDTO();
  private ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
  private ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
  private ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
  private ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
  private List<ProductLevel3Logistics> productLevel3LogisticsRequest = new ArrayList<>();
  private List<ProductL3CommonImageRequest> commonImages = new ArrayList<>();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    ReflectionTestUtils
        .setField(productLevel3HelperBean, "phoneNumberDetectionRegex", "^(?:\\s+|)((08|(?:(\\+|)628))\\d{9,11})$");

    productCollection = new ProductCollection();
    productCollection.setId(ID);
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductName(PRODUCT_NAME);
    productCollection.setCategoryName(CATEGORY_NAME);
    productCollection.setCategoryCode(CATEGORY_CODE);
    productCollection.setNeedCorrectionNotes(VENDOR_NOTES);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    oldPriceDTO.setOfferPrice(OLD_OFFER_PRICE);
    oldPriceDTO.setListPrice(OLD_SALE_PRICE);
    productLevel3PriceRequest.setPrice(OLD_OFFER_PRICE);
    productLevel3PriceRequest.setSalePrice(OLD_SALE_PRICE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    itemViewConfigDTO.setBuyable(false);
    itemViewConfigDTO.setDiscoverable(false);
    quickEditV2Request.setOff2OnActiveFlag(false);
    itemSummaryListResponse.setOff2OnChannelActive(false);

    ProductLevel3AttributeRequest attribute =
      new ProductLevel3AttributeRequest(BUSINESS_PARTNER_CODE, BRAND_CODE, new ArrayList<String>(),
        true);

    ProductL3CommonImageRequest commonImageRequest =
      new ProductL3CommonImageRequest(IMAGE_LOCATION_PATH_1, true, false, "NEW", 0, true);

    productLevel3Logistics.setLogisticProductCode("GOJEK");
    productLevel3Logistics.setSelected(true);

    productLevel3LogisticsRequest.add(productLevel3Logistics);
    productL3UpdateRequest.setProductLevel3LogisticsRequest(productLevel3LogisticsRequest);
    productL3UpdateRequest.setAttributes(Arrays.asList(attribute));
    productL3UpdateRequest.setCommonImages(Arrays.asList(new ProductLevel3SummaryDetailsImageRequest()));
    productL3UpdateRequest.setFreeSample(true);
  }

  private ProductDetailResponse getProductDetailResponseWithImages() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(PRODUCT_CODE);
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(USP);
    productDetailResponse.setDescription(DESCRIPTION.getBytes());
    productDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productDetailResponse.setCategoryCodes(Collections.singletonList(CATEGORY_CODE));

    Image image = new Image();
    image.setLocationPath(IMAGE_LOCATION_PATH_1);
    image.setHashCode(IMAGE_HASH_CODE_1);
    image.setOriginalImage(true);
    Image image1 = new Image();
    image1.setLocationPath(IMAGE_LOCATION_PATH_2);
    image1.setHashCode(IMAGE_HASH_CODE_2);
    image1.setOriginalImage(true);
    Image image2 = new Image();
    image2.setLocationPath(IMAGE_LOCATION_PATH_3);
    image2.setHashCode(IMAGE_HASH_CODE_3);
    image2.setOriginalImage(true);
    Image image3 = new Image();
    image3.setLocationPath(IMAGE_LOCATION_PATH_4);
    image3.setHashCode(IMAGE_HASH_CODE_4);
    image3.setOriginalImage(true);
    Image image4 = new Image();
    image4.setLocationPath(IMAGE_LOCATION_PATH_5);
    image4.setHashCode(IMAGE_HASH_CODE_5);
    image4.setOriginalImage(true);
    image4.setMarkForDelete(true);
    productDetailResponse.setImages(Arrays.asList(image, image1, image2, image3, image4));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(image, image1));
    productItemResponse.setId(ITEM_SKU);
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setImages(Arrays.asList(image2, image1));
    productItemResponse.setId(ITEM_SKU_1);
    productDetailResponse
        .setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse, productItemResponse1)));
    return productDetailResponse;
  }

  @Test
  public void isRestrictedKeywordsPresentInProductDetailsTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName(NAME_WITH_RESTRICTED_KEYWORD);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE))
        .thenReturn(Collections.singletonList(RESTRICTED_KEYWORD));
    List<RestrictedKeywordsByField> response =
        productLevel3HelperBean.getRestrictedKeywordsInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategory(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response));
  }

  @Test
  public void isRestrictedKeywordsPresentInProductDetailsNullTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName(NAME_WITH_RESTRICTED_KEYWORD);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE)).thenReturn(new ArrayList<>());
    List<RestrictedKeywordsByField> response =
        productLevel3HelperBean.getRestrictedKeywordsInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategory(CATEGORY_CODE);
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name restricted");
    productDetailResponse.setUniqueSellingPoint(USP_WITH_RESTRICTED_KEYWORD);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    restrictedKeywordsMappedToCategoryResponse1.setKeywordId(RESTRICTED_KEYWORD_2);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setKeywordId(RESTRICTED_KEYWORD_1);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(), response.getAction());
    Assertions.assertTrue(response.getKeywordToKeywordRequestDTOMap().containsKey(RESTRICTED_KEYWORD_1));
    Assertions.assertTrue(response.getKeywordToKeywordRequestDTOMap().containsKey(RESTRICTED_KEYWORD_2));
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsNoKeywordDetectedTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("");
    productDetailResponse.setUniqueSellingPoint("");
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    restrictedKeywordsMappedToCategoryResponse1.setKeywordId(RESTRICTED_KEYWORD_2);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setKeywordId(RESTRICTED_KEYWORD_1);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsNoKeywordDetectedButEmailTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("abc.@gmail.com");
    productDetailResponse.setUniqueSellingPoint(StringUtils.EMPTY);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    restrictedKeywordsMappedToCategoryResponse1.setKeywordId(RESTRICTED_KEYWORD_2);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setKeywordId(RESTRICTED_KEYWORD_1);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsValidateByDsTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "intelligentRestrictedKeywordsSwitch", true);
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name restricted");
    productDetailResponse.setUniqueSellingPoint(USP_WITH_RESTRICTED_KEYWORD);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setValidateByDs(true);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(-1, response.getAction());
    Assertions.assertTrue(response.isSkipAllActions());
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsValidateByDs1Test() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "intelligentRestrictedKeywordsSwitch", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "updateSkipAllActionsBasedOnAction", true);
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name restricted");
    productDetailResponse.setUniqueSellingPoint(USP_WITH_RESTRICTED_KEYWORD);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setValidateByDs(true);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(-1, response.getAction());
    Assertions.assertTrue(response.isSkipAllActions());
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsValidateByDsSwitchOffTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "intelligentRestrictedKeywordsSwitch", false);
    ReflectionTestUtils.setField(productLevel3HelperBean, "updateSkipAllActionsBasedOnAction", true);
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name restricted");
    productDetailResponse.setUniqueSellingPoint(USP_WITH_RESTRICTED_KEYWORD);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);
    restrictedKeywordsMappedToCategoryResponse2.setValidateByDs(true);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(3, response.getAction());
    Assertions.assertFalse(response.isSkipAllActions());
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsWithDefaultActionTypeTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name");
    productDetailResponse.setUniqueSellingPoint(USP);
    productDetailResponse.setDescription("description dm description".getBytes(StandardCharsets.UTF_8));
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(".dm");
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType(), response.getAction());
  }


  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsTest1() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName("name restricted");
    productDetailResponse.setUniqueSellingPoint(USP_WITH_RESTRICTED_KEYWORD);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse1 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse1.setKeyword(RESTRICTED_KEYWORD_2);
    restrictedKeywordsMappedToCategoryResponse1.setAction(3);
    RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse2 =
        new RestrictedKeywordsMappedToCategoryResponse();
    restrictedKeywordsMappedToCategoryResponse2.setKeyword(RESTRICTED_KEYWORD_1);
    restrictedKeywordsMappedToCategoryResponse2.setAction(2);

    when(productOutbound.getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE)).thenReturn(
        Arrays.asList(restrictedKeywordsMappedToCategoryResponse1, restrictedKeywordsMappedToCategoryResponse2));
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(response.getRestrictedKeywordsByFieldList()));
    Assertions.assertEquals(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(), response.getAction());
  }

  @Test
  public void isRestrictedKeywordsWithActionTypeInProductDetailsNullTest() {
    productCollection.setPostLive(false);
    ProductDetailResponse productDetailResponse = getProductDetailResponseWithImages();
    productDetailResponse.setName(NAME_WITH_RESTRICTED_KEYWORD);
    when(productOutbound.getRestrictedKeywordMappedToCategory(CATEGORY_CODE)).thenReturn(new ArrayList<>());
    RestrictedKeywordsByFieldAndActionType response =
        productLevel3HelperBean.getRestrictedKeywordsWithActionTypeInProductDetails(productDetailResponse, CATEGORY_CODE);
    verify(productOutbound).getRestrictedKeywordMappedToCategoryWithAction(CATEGORY_CODE);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdatePriceChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(500.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateSalePriceChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(500.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateStatusChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.OFFLINE);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateStatusChange2Test() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(false);
    itemSummaryResponse.setDiscoverable(false);
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateStatusChange3Test() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(false);
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateStatusChange4Test() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(false);
    itemSummaryResponse.setDiscoverable(true);
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateOff2OnFlagChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    itemSummaryResponse.setOff2OnChannelActive(true);
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    quickEditRequest.setOff2OnActiveFlag(false);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateSellerSkuChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    itemSummaryResponse.setOff2OnChannelActive(true);
    itemSummaryResponse.setMerchantSku("merchant-sku");
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    quickEditRequest.setOff2OnActiveFlag(true);
    quickEditRequest.setSellerSku("merchant-sku-1");
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdatePickupPointChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    itemSummaryResponse.setOff2OnChannelActive(true);
    itemSummaryResponse.setMerchantSku("merchant-sku");
    itemSummaryResponse.setPickupPointCode("pickup-point");
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    quickEditRequest.setOff2OnActiveFlag(true);
    quickEditRequest.setSellerSku(null);
    quickEditRequest.setPickupPointCode("pickup-point-1");
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateWholsaleChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    itemSummaryResponse.setOff2OnChannelActive(true);
    itemSummaryResponse.setMerchantSku("merchant-sku");
    itemSummaryResponse.setPickupPointCode("pickup-point");
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    quickEditRequest.setOff2OnActiveFlag(true);
    quickEditRequest.setSellerSku("merchant-sku");
    quickEditRequest.setPickupPointCode(null);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, true);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductItemDetailChangedForListingUpdateNoChangeTest() {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setOfferPrice(1000.0);
    priceDTO.setListPrice(1000.0);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(1000.0);
    productLevel3PriceRequest.setSalePrice(1000.0);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setPrice(ImmutableSet.of(priceDTO));
    itemSummaryResponse.setBuyable(true);
    itemSummaryResponse.setDiscoverable(true);
    itemSummaryResponse.setOff2OnChannelActive(true);
    itemSummaryResponse.setMerchantSku("merchant-sku");
    itemSummaryResponse.setPickupPointCode("pickup-point");
    QuickEditRequest quickEditRequest = new QuickEditRequest();
    quickEditRequest.setPrice(productLevel3PriceRequest);
    quickEditRequest.setStatus(ProductLevel3Status.ONLINE);
    quickEditRequest.setOff2OnActiveFlag(true);
    quickEditRequest.setSellerSku("merchant-sku");
    quickEditRequest.setPickupPointCode(null);
    boolean result = productLevel3HelperBean
        .isProductItemDetailChangedForListingUpdate(quickEditRequest, itemSummaryResponse, null);
    Assertions.assertFalse(result);
  }

  @Test
  public void setProductLevel3DetailsFromSummaryResponseTest() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productLevel3HelperBean.setProductLevel3DetailsFromSummaryResponse(productBusinessPartner, productLevel3);
    Assertions.assertEquals(PRODUCT_NAME, productBusinessPartner.getProductName());
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_B2bFiledsUpdateTest() {
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncActive(false);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response = productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_B2bFiledsStatusTest() {
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncActive(false);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    ItemViewConfigDTO itemViewConfigDTO1 = new ItemViewConfigDTO();
    itemViewConfigDTO1.setChannel(Constants.B2B_CHANNEL);
    itemViewConfigDTO1.setBuyable(true);
    itemSummaryListResponse.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO1, itemViewConfigDTO)));
    boolean response = productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_B2bFiledsManangedTest() {
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setStatus(ProductLevel3Status.OFFLINE);
    b2bFieldsRequest.setManaged(true);
    quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
    B2bFieldsDTO b2bFieldsDTO = new B2bFieldsDTO();
    itemSummaryListResponse.setB2bFields(b2bFieldsDTO);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncActive(false);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    ItemViewConfigDTO itemViewConfigDTO1 = new ItemViewConfigDTO();
    itemViewConfigDTO1.setChannel(Constants.B2B_CHANNEL);
    itemSummaryListResponse.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO1, itemViewConfigDTO)));
    boolean response = productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_B2bFiledsBasePriceTest() {
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setStatus(ProductLevel3Status.OFFLINE);
    b2bFieldsRequest.setBasePrice(200.0);
    quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
    B2bFieldsDTO b2bFieldsDTO = new B2bFieldsDTO();
    itemSummaryListResponse.setB2bFields(b2bFieldsDTO);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncActive(false);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    ItemViewConfigDTO itemViewConfigDTO1 = new ItemViewConfigDTO();
    itemViewConfigDTO1.setChannel(Constants.B2B_CHANNEL);
    itemSummaryListResponse.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO1, itemViewConfigDTO)));
    boolean response = productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_priceChangeTest() {
    productLevel3PriceRequest.setPrice(NEW_OFFER_PRICE);
    productLevel3PriceRequest.setSalePrice(NEW_SALE_PRICE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_viewConfigChangeTest() {
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_cncViewConfigChangeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "cncForWarehouseFeatureSwitch", true);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemViewConfigDTO.setChannel(Constants.CNC_CHANNEL);
    quickEditV2Request.setCncStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
        productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
            itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_existingNullCncViewConfigChangeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "cncForWarehouseFeatureSwitch", true);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    quickEditV2Request.setCncStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setItemViewConfigs(new HashSet<>());
    boolean response =
        productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
            itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_cncActivatedChangeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "cncForWarehouseFeatureSwitch", false);
    quickEditV2Request.setCncActive(false);
    itemSummaryListResponse.setCncActive(true);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemViewConfigDTO.setChannel(Constants.CNC_CHANNEL);
    quickEditV2Request.setCncStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
        productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
            itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_switchOnNoChangeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "cncForWarehouseFeatureSwitch", true);
    quickEditV2Request.setCncActive(false);
    itemSummaryListResponse.setCncActive(false);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemViewConfigDTO.setChannel(Constants.CNC_CHANNEL);
    quickEditV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
        productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
            itemSummaryListResponse, false);
    Assertions.assertFalse(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_O2OFlagChangeTest() {
    quickEditV2Request.setOff2OnActiveFlag(true);
    itemSummaryListResponse.setOff2OnChannelActive(false);
    productLevel3PriceRequest.setPrice(OLD_SALE_PRICE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_sellerSkuTest() {
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setSellerSku(NEW_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_wholesaleActivatedTest() {
    itemSummaryListResponse.setWholesalePriceActivated(true);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertTrue(response);
  }

  @Test
  public void isProductItemDetailChangedForL5ListingUpdate_noChangeTest() {
    quickEditV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
    itemSummaryListResponse.setWholesalePriceActivated(false);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncActive(false);
    quickEditV2Request.setSellerSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setMerchantSku(OLD_SELLER_SKU);
    itemSummaryListResponse.setPrice(Collections.singleton(oldPriceDTO));
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    boolean response =
      productLevel3HelperBean.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
        itemSummaryListResponse, false);
    Assertions.assertFalse(response);
  }

  @Test
  public void generateProductLevel3Test(){
    ProductLevel3 productLevel3 = new ProductLevel3();
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PreOrderRequest preOrderRequest = new PreOrderRequest();
    preOrderRequest.setIsPreOrder(true);
    preOrderRequest.setPreOrderDate(new Date());
    preOrderRequest.setPreOrderValue(1);
    preOrderRequest.setPreOrderType(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setPreOrder(preOrderRequest);
    productLevel3 = productLevel3HelperBean.generateProductLevel3(productL3UpdateRequest);
    Assertions.assertEquals(productLevel3.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(productLevel3.getPreOrder().getIsPreOrder());
    Assertions.assertTrue(Objects.nonNull(productLevel3.getPreOrder().getPreOrderDate()));
    Assertions.assertEquals(1, productLevel3.getPreOrder().getPreOrderValue(), 0);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productLevel3.getPreOrder().getPreOrderType());
  }

  @Test
  public void generateProductLevel3AddDeleteVariantsTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", BRAND_CODE);
    ReflectionTestUtils.setField(productLevel3HelperBean, "addDeleteVariantSwitch", true);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(BUSINESS_PARTNER_CODE, "Red");
    attributesMap.put(BRAND_CODE, "Red");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productVariantPriceStockAndImagesRequest.getImages().add(productLevel3SummaryDetailsImageRequest);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setReviewType(NEW_SELLER_SKU);
    productVariantPriceStockAndImagesRequest.getImages().add(productLevel3SummaryDetailsImageRequest1);
    ProductLevel3 productLevel3 = new ProductLevel3();
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.getProductItems().add(productVariantPriceStockAndImagesRequest);
    productL3UpdateRequest.getProductItems().add(new ProductVariantPriceStockAndImagesRequest());
    productLevel3 = productLevel3HelperBean.generateProductLevel3(productL3UpdateRequest);
    Assertions.assertEquals(productLevel3.getBusinessPartnerCode(), BUSINESS_PARTNER_CODE);
  }

  @Test
  public void generateProductLevel3AddDeleteVariantsWithAttributesMapKeyAsNullTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", BRAND_CODE);
    ReflectionTestUtils.setField(productLevel3HelperBean, "addDeleteVariantSwitch", true);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(BUSINESS_PARTNER_CODE, "Red");
    attributesMap.put(BRAND_CODE, "Red");
    attributesMap.put("undefined", "BLACK");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productVariantPriceStockAndImagesRequest.getImages().add(productLevel3SummaryDetailsImageRequest);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest1 =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest1.setReviewType(NEW_SELLER_SKU);
    productVariantPriceStockAndImagesRequest.getImages().add(productLevel3SummaryDetailsImageRequest1);
    ProductLevel3 productLevel3 = new ProductLevel3();
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.getProductItems().add(productVariantPriceStockAndImagesRequest);
    productL3UpdateRequest.getProductItems().add(new ProductVariantPriceStockAndImagesRequest());
    try {
      productLevel3HelperBean.generateProductLevel3(productL3UpdateRequest);
    } catch (ApplicationRuntimeException exception) {
      Assertions.assertEquals(ErrorCategory.VALIDATION.getMessage() + ApiErrorCode.INVALID_ADD_DELETE_REQUEST.getDesc(),
          exception.getErrorMessage());
    }
  }

  @Test
  public void generateProductLevel3_EmptyTest(){
    ProductLevel3 productLevel3 = new ProductLevel3();
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductLevel3LogisticsRequest(new ArrayList<>());
    productLevel3 = productLevel3HelperBean.generateProductLevel3(productL3UpdateRequest);
    Assertions.assertEquals(productLevel3.getBusinessPartnerCode(),BUSINESS_PARTNER_CODE);
  }

  @Test
  public void addBundleRecipeInWMSTest() throws Exception {
    ProductDTO productDTO = new ProductDTO();
    productDTO.setBundleProduct(true);
    ItemActivationRequest itemActivationRequest = new ItemActivationRequest();
    itemActivationRequest.setItemSku(ITEM_SKU);
    itemActivationRequest.setItemCode(ITEM_CODE_1);
    itemActivationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeVo(ITEM_SKU_1, 1)));
    ProductAndItemActivationRequest productAndItemActivationRequest =
        new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest));

    ItemBasicDetailV2Response itemSummaryResponse = new ItemBasicDetailV2Response();
    itemSummaryResponse.setItemSku(ITEM_SKU_1);
    itemSummaryResponse.setItemCode(ITEM_CODE_2);

    ItemBasicDetailV2Response itemSummaryResponse1 = new ItemBasicDetailV2Response();
    itemSummaryResponse1.setItemSku(ITEM_SKU_1);
    itemSummaryResponse1.setItemCode(ITEM_CODE_2);
    itemSummaryResponse1.setMarkForDelete(true);

    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(anyList(), eq(false)))
        .thenReturn(Arrays.asList(itemSummaryResponse, itemSummaryResponse1));

    Mockito.when(wareHouseOutBound.createAndUpdateProductBundle(
        createUpdateBillOfMaterialRecipeRequestArgumentCaptor.capture())).thenReturn(new CreateUpdateBOMRecipeResponse());

    productLevel3HelperBean.addBundleRecipeInWMS(productAndItemActivationRequest);

    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(anyList(), eq(false));
    Mockito.verify(wareHouseOutBound)
        .createAndUpdateProductBundle(createUpdateBillOfMaterialRecipeRequestArgumentCaptor.capture());

    Assertions.assertEquals(ITEM_CODE_1, createUpdateBillOfMaterialRecipeRequestArgumentCaptor.getValue().getItemCode());
    Assertions.assertEquals(1,
        createUpdateBillOfMaterialRecipeRequestArgumentCaptor.getValue().getBillOfMaterialSetup().size());
    Assertions.assertEquals(ITEM_CODE_2,
        createUpdateBillOfMaterialRecipeRequestArgumentCaptor.getValue().getBillOfMaterialSetup().iterator().next()
            .getItemCode());
  }

  @Test
  public void addBundleRecipeInWMSNoBundleProductTest() throws Exception {
    ProductDTO productDTO = new ProductDTO();
    productDTO.setBundleProduct(false);
    ItemActivationRequest itemActivationRequest = new ItemActivationRequest();
    itemActivationRequest.setItemSku(ITEM_SKU);
    itemActivationRequest.setItemCode(ITEM_CODE_1);
    ProductAndItemActivationRequest productAndItemActivationRequest =
        new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest));

    ItemSummaryListResponse itemSummaryResponse = new ItemSummaryListResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU_1);
    itemSummaryResponse.setItemCode(ITEM_CODE_2);

    productLevel3HelperBean.addBundleRecipeInWMS(productAndItemActivationRequest);
  }

  @Test
  public void addBundleRecipeInWMSExpectionTest() throws Exception {
    ProductDTO productDTO = new ProductDTO();
    productDTO.setBundleProduct(true);
    ItemActivationRequest itemActivationRequest = new ItemActivationRequest();
    itemActivationRequest.setItemSku(ITEM_SKU);
    itemActivationRequest.setItemCode(ITEM_CODE_1);
    itemActivationRequest.setBundleRecipe(ImmutableSet.of(new BundleRecipeVo(ITEM_SKU_1, 1)));
    ProductAndItemActivationRequest productAndItemActivationRequest =
        new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest));

    ItemBasicDetailV2Response itemSummaryResponse = new ItemBasicDetailV2Response();
    itemSummaryResponse.setItemSku(ITEM_SKU_1);
    itemSummaryResponse.setItemCode(ITEM_CODE_2);

    ItemBasicDetailV2Response itemSummaryResponse1 = new ItemBasicDetailV2Response();
    itemSummaryResponse1.setItemSku(ITEM_SKU_1);
    itemSummaryResponse1.setItemCode(ITEM_CODE_2);
    itemSummaryResponse1.setMarkForDelete(true);

    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(anyList(), eq(false)))
        .thenReturn(Arrays.asList(itemSummaryResponse, itemSummaryResponse1));

    productLevel3HelperBean.addBundleRecipeInWMS(productAndItemActivationRequest);

    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(anyList(), eq(false));

  }


  @Test
  public void autoFillFamilyColourAttributeSwitchOnDefiningWarnaTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);

    Assertions.assertEquals(2, productVariantPriceStockAndImagesRequest.getAttributesMap().size());
    Assertions.assertEquals(StringUtils.EMPTY, productVariantPriceStockAndImagesRequest.getAttributesMap().get(FAMILY_COLOUR_ATTRIBUTE_CODE));
  }

  @Test
  public void autoFillFamilyColourAttributeSwitchOnDescriptiveWarnaTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3AttributeRequest1.setVariantCreation(true);

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);

    Assertions.assertEquals(2, productVariantPriceStockAndImagesRequest.getAttributesMap().size());
    Assertions.assertEquals(StringUtils.EMPTY, productVariantPriceStockAndImagesRequest.getAttributesMap().get(FAMILY_COLOUR_ATTRIBUTE_CODE));

  }

  @Test
  public void autoFillFamilyColourAttributeSwitchOnWarnaNonVariantCreatingTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Assertions.assertEquals(1, productVariantPriceStockAndImagesRequest.getAttributesMap().size());

  }

  @Test
  public void autoFillFamilyColourAttributeSwitchOnWarnaNotPresentTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(ATTRIBUTE_NAME);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Assertions.assertEquals(1, productVariantPriceStockAndImagesRequest.getAttributesMap().size());

  }

  @Test
  public void autoFillFamilyColourAttributeSwitchOnFamilyColourInItemTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);
    attributeMap.put(FAMILY_COLOUR_ATTRIBUTE_CODE, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Assertions.assertEquals(2, productVariantPriceStockAndImagesRequest.getAttributesMap().size());

  }

  @Test
  public void autoFillFamilyColourAttributeSwitchNoNewlyAddedItemTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);
    attributeMap.put(FAMILY_COLOUR_ATTRIBUTE_CODE, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(false);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Assertions.assertEquals(2, productVariantPriceStockAndImagesRequest.getAttributesMap().size());

  }

  @Test
  public void autoFillFamilyColourAttributeSwitchOnCategoryNotHavingFamilyColourTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", true);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3AttributeRequest1.setVariantCreation(true);

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_NAME);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);

    Assertions.assertEquals(1, productVariantPriceStockAndImagesRequest.getAttributesMap().size());
  }


  @Test
  public void autoFillFamilyColourAttributeSwitchOff() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "autoFillFamilyColourAttribute", false);
    ReflectionTestUtils.setField(productLevel3HelperBean, "familyColorAttributeCode", FAMILY_COLOUR_ATTRIBUTE_CODE);

    ProductLevel3AttributeRequest productLevel3AttributeRequest1 = new ProductLevel3AttributeRequest();
    productLevel3AttributeRequest1.setAttributeName(Constants.WARNA);
    productLevel3AttributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());

    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(Constants.WARNA, RED);
    attributeMap.put(FAMILY_COLOUR_ATTRIBUTE_CODE, RED);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(false);

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(Arrays.asList(productLevel3AttributeRequest1));
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setCategoryCode(CATEGORY_CODE);

    productLevel3HelperBean.autoFillFamilyColourAttribute(productL3UpdateRequest);

    Assertions.assertEquals(2, productVariantPriceStockAndImagesRequest.getAttributesMap().size());

  }

  @Test
  public void validateShareProductRecipeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "sharedProductBundleRecipeEditEnabled", true);

    ProductBundleRecipeRequest productBundleRecipeRequest1 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest1.setItemSku(ITEM_SKU);
    productBundleRecipeRequest1.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU, 1)));

    ProductBundleRecipeRequest productBundleRecipeRequest2 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest2.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest2.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU_1, 1)));

    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setProductBundleRecipe(Arrays.asList(productBundleRecipeRequest1, productBundleRecipeRequest2));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse1 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse1.setItemCode(ITEM_CODE_1);
    sharedProductBundleRecipeResponse1.setSharedProduct(true);
    sharedProductBundleRecipeResponse1.setBundleRecipe(Set.of(new SkuCodeBundleRecipeResponse(ITEM_CODE_1, 1)));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse2 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse2.setItemCode(ITEM_CODE_2);

    Map<String, String> itemSkuAndItemCodeMapping = Map.of(ITEM_SKU, ITEM_CODE_1);

    Mockito.when(xProductOutbound.getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1)))
        .thenReturn(Arrays.asList(sharedProductBundleRecipeResponse1, sharedProductBundleRecipeResponse2));

    List<ProductBundleRecipeRequest> productBundleRecipeRequests =
        productLevel3HelperBean.validateShareProductRecipe(request, itemSkuAndItemCodeMapping);

    Mockito.verify(xProductOutbound).getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1));

    Assertions.assertEquals(1, productBundleRecipeRequests.size());
  }

  @Test
  public void validateShareProductRecipeRecipeMismatchTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "sharedProductBundleRecipeEditEnabled", true);

    ProductBundleRecipeRequest productBundleRecipeRequest1 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest1.setItemSku(ITEM_SKU);
    productBundleRecipeRequest1.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU, 1)));

    ProductBundleRecipeRequest productBundleRecipeRequest2 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest2.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest2.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU_1, 1)));

    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setProductBundleRecipe(Arrays.asList(productBundleRecipeRequest1, productBundleRecipeRequest2));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse1 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse1.setItemCode(ITEM_CODE_1);
    sharedProductBundleRecipeResponse1.setSharedProduct(true);
    sharedProductBundleRecipeResponse1.setBundleRecipe(Set.of(new SkuCodeBundleRecipeResponse(ITEM_CODE_1, 2)));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse2 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse2.setItemCode(ITEM_CODE_2);

    Map<String, String> itemSkuAndItemCodeMapping = Map.of(ITEM_SKU, ITEM_CODE_1);

    Mockito.when(xProductOutbound.getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1)))
        .thenReturn(Arrays.asList(sharedProductBundleRecipeResponse1, sharedProductBundleRecipeResponse2));

    List<ProductBundleRecipeRequest> productBundleRecipeRequests =
          productLevel3HelperBean.validateShareProductRecipe(request, itemSkuAndItemCodeMapping);

    Mockito.verify(xProductOutbound).getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1));

  }

  @Test
  public void validateShareProductRecipeSwitchOffTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "sharedProductBundleRecipeEditEnabled", false);

    ProductBundleRecipeRequest productBundleRecipeRequest1 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest1.setItemSku(ITEM_SKU);
    productBundleRecipeRequest1.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU, 1)));

    ProductBundleRecipeRequest productBundleRecipeRequest2 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest2.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest2.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU_1, 1)));

    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setProductBundleRecipe(Arrays.asList(productBundleRecipeRequest1, productBundleRecipeRequest2));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse1 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse1.setItemCode(ITEM_CODE_1);
    sharedProductBundleRecipeResponse1.setSharedProduct(true);
    sharedProductBundleRecipeResponse1.setBundleRecipe(Set.of(new SkuCodeBundleRecipeResponse(ITEM_CODE_1, 2)));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse2 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse2.setItemCode(ITEM_CODE_2);

    Map<String, String> itemSkuAndItemCodeMapping = Map.of(ITEM_SKU, ITEM_CODE_1);

    List<ProductBundleRecipeRequest> productBundleRecipeRequests =
        productLevel3HelperBean.validateShareProductRecipe(request, itemSkuAndItemCodeMapping);

    Assertions.assertEquals(2, productBundleRecipeRequests.size());
  }

  @Test
  public void validateShareProductRecipeEmptySharedProductRecipeTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "sharedProductBundleRecipeEditEnabled", true);

    ProductBundleRecipeRequest productBundleRecipeRequest1 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest1.setItemSku(ITEM_SKU);
    productBundleRecipeRequest1.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU, 1)));

    ProductBundleRecipeRequest productBundleRecipeRequest2 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest2.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest2.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU_1, 1)));

    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setProductBundleRecipe(Arrays.asList(productBundleRecipeRequest1, productBundleRecipeRequest2));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse1 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse1.setItemCode(ITEM_CODE_1);
    sharedProductBundleRecipeResponse1.setSharedProduct(true);

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse2 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse2.setItemCode(ITEM_CODE_2);

    Map<String, String> itemSkuAndItemCodeMapping = Map.of(ITEM_SKU, ITEM_CODE_1);

    Mockito.when(xProductOutbound.getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1)))
        .thenReturn(Arrays.asList(sharedProductBundleRecipeResponse1, sharedProductBundleRecipeResponse2));

    List<ProductBundleRecipeRequest> productBundleRecipeRequests =
        productLevel3HelperBean.validateShareProductRecipe(request, itemSkuAndItemCodeMapping);

    Mockito.verify(xProductOutbound).getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1));

    Assertions.assertEquals(2, productBundleRecipeRequests.size());
  }

  @Test
  public void validateShareProductRecipeEmptyParentItemCodeNotPresentInMapTest() {
    ReflectionTestUtils.setField(productLevel3HelperBean, "sharedProductBundleRecipeEditEnabled", true);

    ProductBundleRecipeRequest productBundleRecipeRequest1 = new ProductBundleRecipeRequest();
    productBundleRecipeRequest1.setItemSku(ITEM_SKU);
    productBundleRecipeRequest1.setBundleRecipe(Set.of(new ProductBundleRecipe(ITEM_SKU, 1)));

    ProductL3UpdateRequest request = new ProductL3UpdateRequest();
    request.setProductBundleRecipe(Arrays.asList(productBundleRecipeRequest1));

    SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse1 = new SharedProductBundleRecipeResponse();
    sharedProductBundleRecipeResponse1.setItemCode(ITEM_CODE_2);
    sharedProductBundleRecipeResponse1.setSharedProduct(true);

    Map<String, String> itemSkuAndItemCodeMapping = Map.of(ITEM_SKU, ITEM_CODE_1);

    Mockito.when(xProductOutbound.getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1)))
        .thenReturn(Arrays.asList(sharedProductBundleRecipeResponse1));

    List<ProductBundleRecipeRequest> productBundleRecipeRequests =
        productLevel3HelperBean.validateShareProductRecipe(request, itemSkuAndItemCodeMapping);

    Mockito.verify(xProductOutbound).getSharedProductBundleRecipeDetails(Set.of(ITEM_CODE_1));

    Assertions.assertEquals(1, productBundleRecipeRequests.size());
  }

}
