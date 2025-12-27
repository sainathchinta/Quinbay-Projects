package com.gdn.mta.product.util.validator;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class RestrictedKeywordValidatorTest {
  private static final String KEYWORD = "abcd";
  private static final String KEYWORD_WITH_DOT = "JD.ID";
  private static final String KEYWORD_WITH_SPECIAL_CHARS_1 = "+productName";
  private static final String KEYWORD_WITH_SPECIAL_CHARS_2 = "*){desc}";
  private static final String KEYWORD_WITH_SPECIAL_CHARS_3 = "?unique selling point + ";
  private static final String NOTES = "notes";
  private static final String UNIQUE_SELLING_POINT = "unique selling point";
  private static final String PRODUCT_NAME_WITH_RESTRICTED_KEYWORD = "KABEL DATA JD.ID REMAX FAST CHARGING MICRO USB";
  private static final String PRODUCT_NAME = "productName";
  private static final String DEFAULT_PRODUCT_DESCRIPTION = "desc";

  private ProductDetailResponse productDetailResponse;

  @BeforeEach
  public void setUp() {
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    productDetailResponse.setDescription(DEFAULT_PRODUCT_DESCRIPTION.getBytes());
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailUSPTest() {
    List<String> keywordList = Arrays.asList(KEYWORD, UNIQUE_SELLING_POINT, NOTES);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordWithDotPresentInProductNameTest() {
    List<String> keywordList = Arrays.asList(KEYWORD_WITH_DOT, PRODUCT_NAME, NOTES);
    productDetailResponse.setName(PRODUCT_NAME_WITH_RESTRICTED_KEYWORD);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailNameTest() {
    List<String> keywordList = Arrays.asList(KEYWORD, PRODUCT_NAME, NOTES);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailDescriptionTest() {
    List<String> keywordList = Arrays.asList(KEYWORD, DEFAULT_PRODUCT_DESCRIPTION, NOTES);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailNoMatchTest() {
    List<String> keywordList = Arrays.asList(KEYWORD, NOTES);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailDanglingCharMatchTest() {
    List<String> keywordList = Arrays.asList(KEYWORD_WITH_SPECIAL_CHARS_1, NOTES, KEYWORD);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailDanglingCharMatchTest2() {
    List<String> keywordList = Arrays.asList(NOTES, KEYWORD_WITH_SPECIAL_CHARS_2, KEYWORD);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isRestrictedKeywordPresentInProductDetailDanglingCharMatchTest3() {
    List<String> keywordList = Arrays.asList(NOTES, KEYWORD_WITH_SPECIAL_CHARS_3, KEYWORD);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList =
        RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(keywordList, productDetailResponse);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }
}
