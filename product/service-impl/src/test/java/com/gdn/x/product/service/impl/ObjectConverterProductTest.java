package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicMasterDataItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class ObjectConverterProductTest {

  private static final String PRODUCT_NAME = "product-name";

  private static final String SPEC_DETAIL = "spec-detail";

  private static final String DESCRIPTION = "description";

  private static final String LONG_DESCRIPTION = "long-description";

  private static final String UNIQUE_SELLING_POINT = "unique-selling-point";

  private static final String PRODUCT_STORY = "product-story";

  private static final boolean BOOLEAN_FALSE = false;

  private static final String URL = "url";

  private static final String UOM = "uom";

  private static final double SHIPPING_WEIGHT = 1.0;

  private static final double LENGTH = 10.0;

  private static final double WIDTH = 50.0;

  private static final double WEIGHT = 30.0;

  private static final String ATTRIBUTE_CODE = "attribute-code";

  private static final double HEIGHT = 100.0;

  private static final Integer SEQUENCE = 1;

  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";

  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE =
      "predefined-allowed-attribute-code";

  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptive-attribute-value";

  private static final DescriptiveAttributeValueType DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE =
      DescriptiveAttributeValueType.SINGLE;

  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE2 = "descriptive-attribute-value-2";

  private static final DescriptiveAttributeValueType DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE2 =
      DescriptiveAttributeValueType.PREDEFINED;

  private static final String ALLOWED_ATTRIBUTE_CODE2 = "allowed-attribute-code-2";

  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE2 =
      "predefined-allowed-attribute-code-2";

  private static final String ATTRIBUTE_CODE2 = "attribute-code-2";

  private static final Integer SEQUENCE2 = 2;

  private static final String LOCATION_PATH = "location-path";

  private static final String LOCATION_PATH2 = "location-path-2";

  private static final String CATEGORY_CODE = "cat-code";

  private static final boolean BOOLEAN_TRUE = true;

  private static final String BRAND = "brand";
  public static final String VALUE_ENGLISH = "valueEnglish";

  @InjectMocks
  private ObjectConverterServiceImpl productObjectConverterServiceImpl;

  @Mock
  private ItemHelperServiceImpl itemHelperService;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ObjectMapper objectMapper;

  private PriceDTO price;
  private DiscountPriceDTO discount1;
  private DiscountPrice discountPrice;
  private ObjectMapper mapper = new ObjectMapper();

  private ProductAttributeResponse productAttributeResponse;

  private AttributeResponse attributeResponse;

  private ProductDetailResponse productDetailResponse;

  private ProductAttributeValueResponse productAttributeValueResponse;

  private AllowedAttributeValueResponse allowedAttributeValueResponse;

  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse;

  private ArrayList<ProductAttributeValueResponse> productAttributeValueResponses;

  private ProductAttributeValueResponse productAttributeValueResponse2;

  private AllowedAttributeValueResponse allowedAttributeValueResponse2;

  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse2;

  private AttributeResponse attributeResponse2;

  private ProductAttributeResponse productAttributeResponse2;

  private ArrayList<Image> images;

  private Image image;

  private Image image2;

  private List<ProductAttributeResponse> productAttributeResponses;

  private ProductAndAttributeDetailResponse productAndAttributeDetailResponse;

  private Product product = new Product();
  private BasicItemDTO basicItemDTO = new BasicItemDTO();

  @Test
  public void convertToMasterDataProductTest() {
    ReflectionTestUtils.setField(productObjectConverterServiceImpl,
        "valueTypeAdditionForDefiningAttributes", true);
    productDetailResponse.setReviewPending(true);
    productDetailResponse.setShippingWeight(null);
    productDetailResponse.getProductAttributeResponses().get(0).getAttribute()
        .setSizeAttribute(true);
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().getFirst()
        .getPredefinedAllowedAttributeValue().setValueEn(VALUE_ENGLISH);
    MasterDataProduct result =
        this.productObjectConverterServiceImpl
            .convertToMasterDataProduct(this.productDetailResponse);
    assertEquals(result.getBrand(), this.productDetailResponse.getBrand());
    assertEquals(result.getDescription(),
       new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getShippingWeight(),0.0);
    assertEquals(result.getSpecificationDetail(),
       this.productDetailResponse.getSpecificationDetail());
    assertEquals(result.getDescription(),
       new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getLongDescription(),
       new String(this.productDetailResponse.getLongDescription()));
    assertEquals(result.getUniqueSellingPoint(),
       this.productDetailResponse.getUniqueSellingPoint());
    assertEquals(result.getMasterDataProductAttributes().get(0).getMasterDataProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getValueEn(), VALUE_ENGLISH);
    assertEquals(result.isActivated(),this.productDetailResponse.isActivated());
    assertEquals(result.isViewable(),this.productDetailResponse.isViewable());
    assertEquals(result.getProductStory(),this.productDetailResponse.getProductStory());
    assertEquals(result.getUom(),this.productDetailResponse.getUom());
    assertEquals(result.getUrl(),this.productDetailResponse.getUrl());
    assertEquals(result.getLength(),this.productDetailResponse.getLength());
    assertEquals(result.getWidth(),this.productDetailResponse.getWidth());
    assertEquals(result.getHeight(),this.productDetailResponse.getHeight());
    assertEquals(result.getWeight(),this.productDetailResponse.getWeight());
    assertEquals(result.getMasterDataProductImages().size(),2);
    for (int i = 0; i < result.getMasterDataProductImages().size(); i++) {
      assertEquals(result.getMasterDataProductImages().get(i).getLocationPath(),
         this.productDetailResponse.getImages().get(i).getLocationPath());
      assertEquals(result.getMasterDataProductImages().get(i).getSequence(),
         this.productDetailResponse.getImages().get(i).getSequence());
      assertEquals(result.getMasterDataProductImages().get(i).isMainImage(),
         this.productDetailResponse.getImages().get(i).isMainImages());
      assertEquals(true, result.isReviewPending());
    }
    assertEquals(ATTRIBUTE_CODE,result.getSizeAttributeCode());
  }

  @Test
  public void convertToMasterDataProductRanchTest() {
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "ranchIntegrationEnabled", true);
    productDetailResponse.setReviewPending(true);
    productDetailResponse.setShippingWeight(null);
    productDetailResponse.getProductAttributeResponses().get(0).getAttribute().setSizeAttribute(true);
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().getFirst()
        .getPredefinedAllowedAttributeValue().setValueEn(VALUE_ENGLISH);
    MasterDataProduct result =
        this.productObjectConverterServiceImpl.convertToMasterDataProduct(this.productDetailResponse);
    assertEquals(result.getBrand(), this.productDetailResponse.getBrand());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getShippingWeight(), 0.0);
    assertEquals(result.getSpecificationDetail(), this.productDetailResponse.getSpecificationDetail());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getLongDescription(), new String(this.productDetailResponse.getLongDescription()));
    assertEquals(result.getUniqueSellingPoint(), this.productDetailResponse.getUniqueSellingPoint());
    assertEquals(result.getMasterDataProductAttributes().get(0).getMasterDataProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getValueEn(), VALUE_ENGLISH);
    assertEquals(result.isActivated(), this.productDetailResponse.isActivated());
    assertEquals(result.isViewable(), this.productDetailResponse.isViewable());
    assertEquals(result.getProductStory(), this.productDetailResponse.getProductStory());
    assertEquals(result.getUom(), this.productDetailResponse.getUom());
    assertEquals(result.getUrl(), this.productDetailResponse.getUrl());
    assertEquals(result.getLength(), this.productDetailResponse.getLength());
    assertEquals(result.getWidth(), this.productDetailResponse.getWidth());
    assertEquals(result.getHeight(), this.productDetailResponse.getHeight());
    assertEquals(result.getWeight(), this.productDetailResponse.getWeight());
    assertEquals(result.getMasterDataProductImages().size(), 2);
    for (int i = 0; i < result.getMasterDataProductImages().size(); i++) {
      assertEquals(result.getMasterDataProductImages().get(i).getLocationPath(),
          this.productDetailResponse.getImages().get(i).getLocationPath());
      assertEquals(result.getMasterDataProductImages().get(i).getSequence(),
          this.productDetailResponse.getImages().get(i).getSequence());
      assertEquals(result.getMasterDataProductImages().get(i).isMainImage(),
          this.productDetailResponse.getImages().get(i).isMainImages());
      assertEquals(true, result.isReviewPending());
    }
    assertEquals(ATTRIBUTE_CODE, result.getSizeAttributeCode());
  }

  @Test
  public void convertToMasterDataProductRanchNonNullTest() throws JsonProcessingException {
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "ranchIntegrationEnabled", true);
    productDetailResponse.setReviewPending(true);
    productDetailResponse.setShippingWeight(null);
    productDetailResponse.getProductAttributeResponses().get(0).getAttribute().setSizeAttribute(true);
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().getFirst()
        .getPredefinedAllowedAttributeValue().setValueEn(VALUE_ENGLISH);
    DistributionInfoResponse distributionInfoResponse = new DistributionInfoResponse();
    distributionInfoResponse.setProductName(PRODUCT_NAME);
    productDetailResponse.setDistributionInfoResponse(distributionInfoResponse);
    when(objectMapper.writeValueAsString(productDetailResponse.getDistributionInfoResponse())).thenReturn(
        mapper.writeValueAsString(distributionInfoResponse));
    MasterDataProduct result =
        this.productObjectConverterServiceImpl.convertToMasterDataProduct(this.productDetailResponse);
    verify(objectMapper).writeValueAsString(productDetailResponse.getDistributionInfoResponse());
    assertEquals(result.getBrand(), this.productDetailResponse.getBrand());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getShippingWeight(), 0.0);
    assertEquals(result.getSpecificationDetail(), this.productDetailResponse.getSpecificationDetail());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getLongDescription(), new String(this.productDetailResponse.getLongDescription()));
    assertEquals(result.getUniqueSellingPoint(), this.productDetailResponse.getUniqueSellingPoint());
    assertEquals(result.getMasterDataProductAttributes().get(0).getMasterDataProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getValueEn(), VALUE_ENGLISH);
    assertEquals(result.isActivated(), this.productDetailResponse.isActivated());
    assertEquals(result.isViewable(), this.productDetailResponse.isViewable());
    assertEquals(result.getProductStory(), this.productDetailResponse.getProductStory());
    assertEquals(result.getUom(), this.productDetailResponse.getUom());
    assertEquals(result.getUrl(), this.productDetailResponse.getUrl());
    assertEquals(result.getLength(), this.productDetailResponse.getLength());
    assertEquals(result.getWidth(), this.productDetailResponse.getWidth());
    assertEquals(result.getHeight(), this.productDetailResponse.getHeight());
    assertEquals(result.getWeight(), this.productDetailResponse.getWeight());
    assertEquals(result.getMasterDataProductImages().size(), 2);
    for (int i = 0; i < result.getMasterDataProductImages().size(); i++) {
      assertEquals(result.getMasterDataProductImages().get(i).getLocationPath(),
          this.productDetailResponse.getImages().get(i).getLocationPath());
      assertEquals(result.getMasterDataProductImages().get(i).getSequence(),
          this.productDetailResponse.getImages().get(i).getSequence());
      assertEquals(result.getMasterDataProductImages().get(i).isMainImage(),
          this.productDetailResponse.getImages().get(i).isMainImages());
      assertEquals(true, result.isReviewPending());
    }
    assertEquals(ATTRIBUTE_CODE, result.getSizeAttributeCode());
  }

  @Test
  public void convertToMasterDataProductRanchExceptionTest() throws JsonProcessingException {
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "valueTypeAdditionForDefiningAttributes", true);
    ReflectionTestUtils.setField(productObjectConverterServiceImpl, "ranchIntegrationEnabled", true);
    productDetailResponse.setReviewPending(true);
    productDetailResponse.setShippingWeight(null);
    productDetailResponse.getProductAttributeResponses().get(0).getAttribute().setSizeAttribute(true);
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().getFirst()
        .getPredefinedAllowedAttributeValue().setValueEn(VALUE_ENGLISH);
    DistributionInfoResponse distributionInfoResponse = new DistributionInfoResponse();
    distributionInfoResponse.setProductName(PRODUCT_NAME);
    productDetailResponse.setDistributionInfoResponse(distributionInfoResponse);
    doThrow(JsonProcessingException.class).when(objectMapper).writeValueAsString(productDetailResponse.getDistributionInfoResponse());
    MasterDataProduct result =
        this.productObjectConverterServiceImpl.convertToMasterDataProduct(this.productDetailResponse);
    verify(objectMapper).writeValueAsString(productDetailResponse.getDistributionInfoResponse());
    assertEquals(result.getBrand(), this.productDetailResponse.getBrand());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getShippingWeight(), 0.0);
    assertEquals(result.getSpecificationDetail(), this.productDetailResponse.getSpecificationDetail());
    assertEquals(result.getDescription(), new String(this.productDetailResponse.getDescription()));
    assertEquals(result.getLongDescription(), new String(this.productDetailResponse.getLongDescription()));
    assertEquals(result.getUniqueSellingPoint(), this.productDetailResponse.getUniqueSellingPoint());
    assertEquals(result.getMasterDataProductAttributes().get(0).getMasterDataProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().getValueEn(), VALUE_ENGLISH);
    assertEquals(result.isActivated(), this.productDetailResponse.isActivated());
    assertEquals(result.isViewable(), this.productDetailResponse.isViewable());
    assertEquals(result.getProductStory(), this.productDetailResponse.getProductStory());
    assertEquals(result.getUom(), this.productDetailResponse.getUom());
    assertEquals(result.getUrl(), this.productDetailResponse.getUrl());
    assertEquals(result.getLength(), this.productDetailResponse.getLength());
    assertEquals(result.getWidth(), this.productDetailResponse.getWidth());
    assertEquals(result.getHeight(), this.productDetailResponse.getHeight());
    assertEquals(result.getWeight(), this.productDetailResponse.getWeight());
    assertEquals(result.getMasterDataProductImages().size(), 2);
    for (int i = 0; i < result.getMasterDataProductImages().size(); i++) {
      assertEquals(result.getMasterDataProductImages().get(i).getLocationPath(),
          this.productDetailResponse.getImages().get(i).getLocationPath());
      assertEquals(result.getMasterDataProductImages().get(i).getSequence(),
          this.productDetailResponse.getImages().get(i).getSequence());
      assertEquals(result.getMasterDataProductImages().get(i).isMainImage(),
          this.productDetailResponse.getImages().get(i).isMainImages());
      assertEquals(true, result.isReviewPending());
    }
    assertEquals(ATTRIBUTE_CODE, result.getSizeAttributeCode());
  }

  @Test
  public void convertToMasterDataProductTestWithNullProductDetailResponse() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.productObjectConverterServiceImpl.convertToMasterDataProduct(null));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.attributeResponse = new AttributeResponse();
    this.attributeResponse.setAttributeCode(ObjectConverterProductTest.ATTRIBUTE_CODE);
    this.attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    this.attributeResponse.setDescription(ObjectConverterProductTest.DESCRIPTION.getBytes());

    this.allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    this.allowedAttributeValueResponse
        .setAllowedAttributeCode(ObjectConverterProductTest.ALLOWED_ATTRIBUTE_CODE);
    this.predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    this.predefinedAllowedAttributeValueResponse
        .setPredefinedAllowedAttributeCode(ObjectConverterProductTest.PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    this.productAttributeValueResponse = new ProductAttributeValueResponse();
    this.productAttributeValueResponse.setAllowedAttributeValue(this.allowedAttributeValueResponse);
    this.productAttributeValueResponse
        .setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueResponse);
    this.productAttributeValueResponse
        .setDescriptiveAttributeValue(ObjectConverterProductTest.DESCRIPTIVE_ATTRIBUTE_VALUE);
    this.productAttributeValueResponse
        .setDescriptiveAttributeValueType(ObjectConverterProductTest.DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE);

    this.allowedAttributeValueResponse2 = new AllowedAttributeValueResponse();
    this.allowedAttributeValueResponse2
        .setAllowedAttributeCode(ObjectConverterProductTest.ALLOWED_ATTRIBUTE_CODE2);
    this.predefinedAllowedAttributeValueResponse2 = new PredefinedAllowedAttributeValueResponse();
    this.predefinedAllowedAttributeValueResponse2
        .setPredefinedAllowedAttributeCode(ObjectConverterProductTest.PREDEFINED_ALLOWED_ATTRIBUTE_CODE2);
    this.productAttributeValueResponse2 = new ProductAttributeValueResponse();
    this.productAttributeValueResponse2
        .setAllowedAttributeValue(this.allowedAttributeValueResponse2);
    this.productAttributeValueResponse2
        .setPredefinedAllowedAttributeValue(this.predefinedAllowedAttributeValueResponse2);
    this.productAttributeValueResponse2
        .setDescriptiveAttributeValue(ObjectConverterProductTest.DESCRIPTIVE_ATTRIBUTE_VALUE2);
    this.productAttributeValueResponse2
        .setDescriptiveAttributeValueType(ObjectConverterProductTest.DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE2);

    this.productAttributeValueResponses = new ArrayList<ProductAttributeValueResponse>();
    this.productAttributeValueResponses.add(this.productAttributeValueResponse);
    this.productAttributeValueResponses.add(this.productAttributeValueResponse2);

    this.productAttributeResponse = new ProductAttributeResponse();
    this.productAttributeResponse.setAttribute(this.attributeResponse);
    this.productAttributeResponse.setOwnByProductItem(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.productAttributeResponse.setSequence(ObjectConverterProductTest.SEQUENCE);
    this.productAttributeResponse.setProductAttributeValues(this.productAttributeValueResponses);

    this.attributeResponse2 = new AttributeResponse();
    this.attributeResponse2.setAttributeCode(ObjectConverterProductTest.ATTRIBUTE_CODE2);
    this.attributeResponse2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    this.attributeResponse2.setDescription(ObjectConverterProductTest.DESCRIPTION.getBytes());

    this.productAttributeResponse2 = new ProductAttributeResponse();
    this.productAttributeResponse2.setAttribute(this.attributeResponse2);
    this.productAttributeResponse2.setOwnByProductItem(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.productAttributeResponse2.setSequence(ObjectConverterProductTest.SEQUENCE2);
    this.productAttributeResponse2.setProductAttributeValues(this.productAttributeValueResponses);
    this.productAttributeResponses = new ArrayList<ProductAttributeResponse>();
    this.productAttributeResponses.add(this.productAttributeResponse);
    this.productAttributeResponses.add(this.productAttributeResponse2);

    this.image = new Image();
    this.image.setLocationPath(ObjectConverterProductTest.LOCATION_PATH);
    this.image.setMainImages(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.image.setSequence(ObjectConverterProductTest.SEQUENCE);

    this.image2 = new Image();
    this.image2.setLocationPath(ObjectConverterProductTest.LOCATION_PATH2);
    this.image2.setMainImages(ObjectConverterProductTest.BOOLEAN_TRUE);
    this.image2.setSequence(ObjectConverterProductTest.SEQUENCE2);

    this.images = new ArrayList<Image>();
    this.images.add(this.image);
    this.images.add(this.image2);

    this.productDetailResponse = new ProductDetailResponse();
    CategoryResponse category = new CategoryResponse();
    category.setCatalog(new CatalogResponse());
    this.productDetailResponse.setProductCategoryResponses(Arrays
        .asList(new ProductCategoryResponse(category, "10001")));
    this.productDetailResponse.setName(ObjectConverterProductTest.PRODUCT_NAME);
    this.productDetailResponse.setBrand(ObjectConverterProductTest.BRAND);
    this.productDetailResponse.setShippingWeight(ObjectConverterProductTest.SHIPPING_WEIGHT);
    this.productDetailResponse.setSpecificationDetail(ObjectConverterProductTest.SPEC_DETAIL);
    this.productDetailResponse.setDescription(ObjectConverterProductTest.DESCRIPTION.getBytes());
    this.productDetailResponse.setLongDescription(ObjectConverterProductTest.LONG_DESCRIPTION
        .getBytes());
    this.productDetailResponse
        .setUniqueSellingPoint(ObjectConverterProductTest.UNIQUE_SELLING_POINT);
    this.productDetailResponse.setActivated(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.productDetailResponse.setViewable(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.productDetailResponse.setProductStory(ObjectConverterProductTest.PRODUCT_STORY);
    this.productDetailResponse.setUom(ObjectConverterProductTest.UOM);
    this.productDetailResponse.setUrl(ObjectConverterProductTest.URL);
    this.productDetailResponse.setLength(ObjectConverterProductTest.LENGTH);
    this.productDetailResponse.setWidth(ObjectConverterProductTest.WIDTH);
    this.productDetailResponse.setHeight(ObjectConverterProductTest.HEIGHT);
    this.productDetailResponse.setWeight(ObjectConverterProductTest.WEIGHT);
    this.productDetailResponse.setProductAttributeResponses(this.productAttributeResponses);
    this.productDetailResponse.setImages(this.images);

    productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    this.productAndAttributeDetailResponse.setName(ObjectConverterProductTest.PRODUCT_NAME);
    this.productAndAttributeDetailResponse.setBrand(ObjectConverterProductTest.BRAND);
    this.productAndAttributeDetailResponse.setShippingWeight(ObjectConverterProductTest.SHIPPING_WEIGHT);
    this.productAndAttributeDetailResponse.setSpecificationDetail(ObjectConverterProductTest.SPEC_DETAIL);
    this.productAndAttributeDetailResponse.setDescription(ObjectConverterProductTest.DESCRIPTION.getBytes());
    this.productAndAttributeDetailResponse.setLongDescription(ObjectConverterProductTest.LONG_DESCRIPTION
        .getBytes());
    this.productAndAttributeDetailResponse
        .setUniqueSellingPoint(ObjectConverterProductTest.UNIQUE_SELLING_POINT);
    this.productAndAttributeDetailResponse.setViewable(ObjectConverterProductTest.BOOLEAN_FALSE);
    this.productAndAttributeDetailResponse.setProductStory(ObjectConverterProductTest.PRODUCT_STORY);
    this.productAndAttributeDetailResponse.setUom(ObjectConverterProductTest.UOM);
    this.productAndAttributeDetailResponse.setUrl(ObjectConverterProductTest.URL);
    this.productAndAttributeDetailResponse.setLength(ObjectConverterProductTest.LENGTH);
    this.productAndAttributeDetailResponse.setWidth(ObjectConverterProductTest.WIDTH);
    this.productAndAttributeDetailResponse.setHeight(ObjectConverterProductTest.HEIGHT);
    this.productAndAttributeDetailResponse.setWeight(ObjectConverterProductTest.WEIGHT);
    this.productAndAttributeDetailResponse.setActivated(ObjectConverterProductTest.BOOLEAN_FALSE);
    productAndAttributeDetailResponse.setProductAttributeResponses(this.productAttributeResponses);
    product.setBrand(BRAND);
    basicItemDTO.setMasterDataItem(new BasicMasterDataItemDTO());
    price = new PriceDTO();
    price.setListPrice(20.00);
    price.setOfferPrice(15.00);
    discount1 = new DiscountPriceDTO();
    discount1.setDiscountPrice(10.00);
    discount1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discount1.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    discount1.setPriority(1);
    price.setListOfDiscountPrices(Collections.singletonList(discount1));
    discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(10.00);
    discountPrice.setPriority(1);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
  }

  @Test
  public void convertProductAndAttributeDetailResponseToMasterDataProductTest(){
    List<MasterDataProductAttribute> masterDataProductAttributes = productObjectConverterServiceImpl
        .convertToMasterDataProductAttribute(productAndAttributeDetailResponse);
    assertNotNull(masterDataProductAttributes);
  }

  @Test
  public void convertToBasicProductAndItemDTOTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.emptySet());
    BasicProductAndItemDTO basicProductAndItemDTO = productObjectConverterServiceImpl
        .convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
   Assertions.assertEquals(product.getProductCode(), basicProductAndItemDTO.getProductCode());
  }

  @Test
  public void convertToBasicProductAndItemDTOUnSyncMasterDataNullTest() {
    product.setSynchronized(false);
    product.setMasterDataProduct(null);
    basicItemDTO.setPrice(Collections.emptySet());
    BasicProductAndItemDTO basicProductAndItemDTO = productObjectConverterServiceImpl
        .convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), null);
  }

  @Test
  public void convertToBasicProductAndItemDTOUnSyncTest() {
    product.setSynchronized(false);
    product.setMasterDataProduct(new MasterDataProduct());
    basicItemDTO.setPrice(Collections.emptySet());
    BasicProductAndItemDTO basicProductAndItemDTO = productObjectConverterServiceImpl
        .convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), null);
  }

  @Test
  public void convertToBasicProductAndItemDTOUnSyncCategoryIssueTest() {
    product.setSynchronized(false);
    product.setMasterDataProduct(new MasterDataProduct());
    basicItemDTO.setPrice(Collections.emptySet());
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    BasicProductAndItemDTO basicProductAndItemDTO = productObjectConverterServiceImpl
        .convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), null);
  }

  @Test
  public void convertToBasicProductAndItemDTOUnSyncAddCategoryTest() {
    product.setSynchronized(false);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct().setBrand(BRAND);
    product.getMasterDataProduct().setShippingWeight(WEIGHT);
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    product.getMasterDataProduct().getMasterCatalog().setCategory(new Category());
    product.getMasterDataProduct().getMasterCatalog().getCategory().setCategoryCode(CATEGORY_CODE);
    basicItemDTO.setPrice(Collections.emptySet());
    BasicProductAndItemDTO basicProductAndItemDTO = productObjectConverterServiceImpl
        .convertToBasicProductAndItemDTO(product, new ArrayList<>(), basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterCatalog().getCategory().getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithPriceValidationTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(discountPrice);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(discountPrice));
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithEmptyDiscountPriceValidationTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    DiscountPrice dp = new DiscountPrice();
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(dp);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(dp))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(dp));
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithNullDiscountPriceValidationTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    DiscountPrice dp = new DiscountPrice();
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(dp);
    when(itemHelperService.processDiscountPricesByPriority(
        Collections.singletonList(dp))).thenReturn(null);
    BasicProductAndItemDTO basicProductAndItemDTO =
        productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
            basicItemDTO);
    Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
        Collections.singletonList(dp));
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithMerchantPromoTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    basicItemDTO.setMerchantPromoDiscount(true);
    discount1.setDiscountPrice(4.00);
    price.setMerchantPromoDiscountPrice(discount1);
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(discountPrice);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(discountPrice));
   Assertions.assertEquals(4.00,
      basicProductAndItemDTO.getItem().getPrice().stream().findFirst().get().getOfferPrice(),0);
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithNullMerchantPromoTest() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    basicItemDTO.setMerchantPromoDiscount(true);
    discount1.setDiscountPrice(4.00);
    price.setMerchantPromoDiscountPrice(null);
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(discountPrice);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(discountPrice));
   Assertions.assertEquals(15.00,
      basicProductAndItemDTO.getItem().getPrice().stream().findFirst().get().getOfferPrice(),0);
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithStartDateValidations() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    basicItemDTO.setMerchantPromoDiscount(true);
    discount1.setDiscountPrice(4.00);
    discount1.setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(1))));
    discount1.setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(30))));
    price.setMerchantPromoDiscountPrice(discount1);
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(discountPrice);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(discountPrice));
   Assertions.assertEquals(15.00,
      basicProductAndItemDTO.getItem().getPrice().stream().findFirst().get().getOfferPrice(),0);
  }

  @Test
  public void convertToBasicProductAndItemDTO_WithEndDateValidations() {
    product.setSynchronized(true);
    basicItemDTO.setPrice(Collections.singleton(price));
    basicItemDTO.setMerchantPromoDiscount(true);
    discount1.setDiscountPrice(4.00);
    discount1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discount1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(30))));
    price.setMerchantPromoDiscountPrice(discount1);
    when(this.gdnMapper.deepCopy(discount1, DiscountPrice.class)).thenReturn(discountPrice);
    when(itemHelperService.processDiscountPricesByPriority(
      Collections.singletonList(discountPrice))).thenReturn(discountPrice);
    BasicProductAndItemDTO basicProductAndItemDTO =
      productObjectConverterServiceImpl.convertToBasicProductAndItemDTO(product, new ArrayList<>(),
        basicItemDTO);
   Assertions.assertEquals(basicProductAndItemDTO.getMasterDataProduct().getBrand(), BRAND);
    verify(itemHelperService).processDiscountPricesByPriority(
      Collections.singletonList(discountPrice));
   Assertions.assertEquals(15.00,
      basicProductAndItemDTO.getItem().getPrice().stream().findFirst().get().getOfferPrice(),0);
  }


  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(itemHelperService);
    verifyNoMoreInteractions(objectMapper);
  }
}
