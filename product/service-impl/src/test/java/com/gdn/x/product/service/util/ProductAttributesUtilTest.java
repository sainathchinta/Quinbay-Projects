package com.gdn.x.product.service.util;

import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.model.entity.PristineDataItem;

public class ProductAttributesUtilTest {

  private static final String PRISTINE_LISTING_ATTRIBUTE =
      "{\"HANDPHONE\" : \"color,rom\", \"COMPUTER\" : \"color\"}";

  private static final String PRISTINE_ATTRIBUTE_VALUE_TRANSLATION =
      "{\"color\":\"Warna\", \"rom\":\"Kapasitas Memori\"}";

  Map<String, String> listingParameters = new HashMap<>();
  Map<String, String> attributeNameTranslationMap = new HashMap<>();


  @InjectMocks
  private ProductAttributesUtilImpl productAttributesUtil;

  @Mock
  ObjectMapper objectMapper;

  private PristineDataItem pristineDataItem;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    listingParameters.put("HANDPHONE", "color,rom");
    listingParameters.put("COMPUTER", "color");

    attributeNameTranslationMap.put("color", "Warna");
    attributeNameTranslationMap.put("rom", "Kapasitas Memori");

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_ATTRIBUTE_VALUE_TRANSLATION), Mockito.any(TypeReference.class)))
        .thenReturn(attributeNameTranslationMap);

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_LISTING_ATTRIBUTE), Mockito.any(TypeReference.class)))
        .thenReturn(listingParameters);


    Map<String, String> pristineListingAttributes = new HashMap<>();
    pristineListingAttributes.put("COLOR", "WHITE");
    pristineListingAttributes.put("ROM", "8 GB");
    pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("PRISTINE_ID");
    pristineDataItem.setPristineCategory("HANDPHONE");
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);

    ReflectionTestUtils.setField(productAttributesUtil,
        "pristineListingParameters", PRISTINE_LISTING_ATTRIBUTE);
    ReflectionTestUtils.setField(productAttributesUtil,
        "pristineProductAttributeNameMap", PRISTINE_ATTRIBUTE_VALUE_TRANSLATION);
    ReflectionTestUtils.invokeMethod(productAttributesUtil, "initValue");
  }

  @Test
  public void getCategoryListingParameterKeyTest() throws Exception {
    String[] expectedCategoryListingParameters = {"color", "rom"};
    String[] result = productAttributesUtil.getCategoryListingParameterKey(pristineDataItem);
    Assertions.assertEquals(Arrays.stream(expectedCategoryListingParameters).toList(), Arrays.stream(result).toList());
  }

  @Test
  public void getCategoryListingParameterKeyTest_whenNoAttributes() throws Exception {
    pristineDataItem.setPristineCategory("RANDOM");
    String[] result = productAttributesUtil.getCategoryListingParameterKey(pristineDataItem);
    Assertions.assertNull(result);
  }

  @Test
  public void translatePristineListingAttributeNameTest() throws IOException {

    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");

    Map<String, String> result = productAttributesUtil
        .translatePristineListingAttributeName(pristineDataItem.getPristineListingAttributes(),
            productAttributesUtil.getCategoryListingParameterKey(pristineDataItem));

    Assertions.assertEquals(expectedPristineAttributes, result);

  }

  @Test
  public void translatePristineListingNullAttributelistTest() throws IOException {
    Map<String, String> result = productAttributesUtil.translatePristineListingAttributeName(null,
        productAttributesUtil.getCategoryListingParameterKey(pristineDataItem));

    Assertions.assertEquals(new HashMap<>(), result);
  }

  @Test
  public void translatePristineListingAttributeNameWithcategoryListingParameterKeyNullTest() throws IOException {
    Map<String, String> result = productAttributesUtil
        .translatePristineListingAttributeName(pristineDataItem.getPristineListingAttributes(), null);

    Assertions.assertEquals(pristineDataItem.getPristineListingAttributes(), result);
  }

  @Test
  public void translatePristineListingAttributeNameWithcategoryListingParameterKeySize0Test() throws IOException {
    Map<String, String> result = productAttributesUtil
        .translatePristineListingAttributeName(pristineDataItem.getPristineListingAttributes(), new String[] {});

    Assertions.assertEquals(pristineDataItem.getPristineListingAttributes(), result);
  }

  @Test
  public void getAttributeNameTranslationMapTest() throws Exception {
    Map<String, String> result = productAttributesUtil.getAttributeNameTranslationMap();
    Assertions.assertEquals(attributeNameTranslationMap, result);
  }
}
