package com.gdn.mta.product.util.validator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class PhoneNumberValidatorTest {

  private static final String PRODUCT_NAME = "productName";
  private static final String UNIQUE_SELLING_POINT = "unique selling point";
  private static final String DEFAULT_PRODUCT_DESCRIPTION = "desc";
  private static final String DEFAULT_ATTRIBUTE_VALUE = "Attribute value";
  private static final String NAME_WITH_VALID_PHONE_NUMBER = "+628878NAME +(628)173-456-8900  08NAME";
  private static final String NAME_WITH_VALID_EMAIL = "NameX123 merchant@blibli.com Name 123@";
  private static final String NAME_WITH_INVALID_EMAIL = "NameX123 mercha@nt@blibli.com Name 123";
  private static final String NAME_WITH_INVALID_PHONE_NUMBER = "12 NAME +628.173 NAME -4567 --8 9045NAME";
  private static final String DESCRIPTION_WITH_INVALID_PHONE_NUMBER = "D456esc +6281734567 --8 0823DESC";
  private static final String DESCRIPTION_WITH_VALID_PHONE_NUMBER = "D456esc +(628)17DESC34567890DESC DESC08";
  private static final String VALID_WORD_PHONE_NUMBER1 = "BliBli Com Sale 08 I Text 1235 seven eight nine snapDragon 845";
  private static final String VALID_WORD_PHONE_NUMBER =
      "123@Name zero eight oNe two thrEE four fIVe six SEVEN Eight@# 9 SNAP 855";
  private static final String INVALID_WORD_PHONE_NUMBER =
      "123@Name zero eight oNe two thrEE four fIVe six SEVEN Eight@# 9 123 SNAP 855";
  private static final String USP_WITH_VALID_PHONE_NUMBER =
      "unique  789 selling point 08123456781 unique selling point";
  private static final String USP_WITH_INVALID_PHONE_NUMBER =
      "unique  789 selling point 95123456781 unique selling point";
  private static final String PHONE_REGEX = "^(?:\\s+|)((08|(?:(\\+|)628))\\d{9,11})$";
  private static final String DESC_BYTES =
      "PCFET0NUWVBFIGh0bWw+CjxodG1sPgo8aGVhZD4KPC9oZWFkPgo8Ym9keT4KPGRpdiBzdHlsZT0iYm94LXNpemluZzogYm9yZGVyLWJveDsgZm9udC1mYW1pbHk6ICdTZWdvZSBVSScsIHN5c3RlbS11aSwgJ0FwcGxlIENvbG9yIEVtb2ppJywgJ1NlZ29lIFVJIEVtb2ppJywgc2Fucy1zZXJpZjsiPjxzcGFuIHN0eWxlPSJmb250LXNpemU6IDEycHg7Ij5ld2Z2cmRzdiBmZHZnZiBkZnZnIGRmZWcgZndldiAzMiA0MzUgNHNkZnYgNDM1IHdmZHMgNSByciBmJm5ic3A7PC9zcGFuPjxhIHN0eWxlPSJmb250LXNpemU6IDEycHg7IiB0YWJpbmRleD0iLTEiIHRpdGxlPSJtYWlsdG86bWt5b25nLjEwMEBta3lvbmcuY29tLmF1IiBocmVmPSJtYWlsdG86bWt5b25nLjEwMEBta3lvbmcuY29tLmF1IiB0YXJnZXQ9Il9ibGFuayIgcmVsPSJub3JlZmVycmVyIG5vb3BlbmVyIj5ta3lvbmcuMTAwQG1reW9uZy5jb20uYXU8L2E+PHNwYW4gc3R5bGU9ImZvbnQtc2l6ZTogMTJweDsiPiAyNDNyIGVkIHIzZWZzZGMgZHNjeCBzZGN4PC9zcGFuPjwvZGl2Pgo8L2JvZHk+CjwvaHRtbD4=";
  private static final String DESC_BYTES2 =
      "PCFET0NUWVBFIGh0bWw+CjxodG1sPgo8aGVhZD4KPC9oZWFkPgo8Ym9keT4KPHA+dGhpcyBpcyBhIHZhbGlkIGVtYWwgaWQgPGEgaHJlZj0ibWFpbHRvOm5lZXR1Lmt1bWFyaUBnbWFpbC5jb20iPm5lZXR1Lmt1bWFyaUBnbWFpbC5jb208L2E+IGpiY2p3YiB3aXRoIHNvbWUgamlicmVpc2o8L3A+CjwvYm9keT4KPC9odG1sPg==";
  private static final String DESC_BYTES3 = "PCFET0NUWVBFIGh0bWw+CjxodG1sPgo8aGVhZD4KPC9oZWFkPgo8Ym9keT4KPGRpdiBzdHlsZT0ibWFyZ2luOiAwcHg7IHBhZGRpbmc6IDBweDsgYm9yZGVyOiAwcHg7IGZvbnQtc2l6ZTogMTZweDsgdmVydGljYWwtYWxpZ246IGJhc2VsaW5lOyBjb2xvcjogIzMzMzMzMzsgZm9udC1mYW1pbHk6ICdTb3VyY2UgU2FucyBQcm8nLCBzYW5zLXNlcmlmOyI+VHlwZSBicmFuZCwgcHJvZHVjdCBuYW1lLCBwcm9kdWN0LWNvZGUgYW5kIHByb2R1Y3QgdHlwZSBhcyBjb21wbGV0ZSBhcyBwb3NzaWJsZSB0byBvYnRhaW4gbW9yZSBhY2N1cmF0ZSByZXN1bHQuPC9kaXY+CjxkaXYgc3R5bGU9Im1hcmdpbjogMHB4OyBwYWRkaW5nOiAwcHg7IGJvcmRlcjogMHB4OyBmb250LXNpemU6IDE2cHg7IHZlcnRpY2FsLWFsaWduOiBiYXNlbGluZTsgY29sb3I6ICMzMzMzMzM7IGZvbnQtZmFtaWx5OiAnU291cmNlIFNhbnMgUHJvJywgc2Fucy1zZXJpZjsiPlRoZXJlIGlzIDIxNSw2NzcgcHJvZHVjdCBsaXN0ZWQgb24gQmxpYmxpLmNvbS4gQnkgdXNpbmcgZGF0YSBmcm9tIHNpbWlsYXIgcHJvZHVjdHMgdGhhdCBhbHJlYWR5IGV4aXN0LCB5b3UgZG8gbm90IG5lZWQgdG8gcmVmaWxsIHRoZSBmZWF0dXJlIGRldGFpbHMgb2YgdGhlIHByb2R1Y3QmbmJzcDs8YSBzdHlsZT0iYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQ7IGNvbG9yOiAjMDA5NWRhOyBjdXJzb3I6IHBvaW50ZXI7IGRpc3BsYXk6IGlubGluZS1ibG9jazsgdGV4dC1kZWNvcmF0aW9uLWxpbmU6IG5vbmU7IG1hcmdpbjogMHB4OyBwYWRkaW5nOiAwcHg7IGJvcmRlcjogMHB4OyB2ZXJ0aWNhbC1hbGlnbjogYmFzZWxpbmU7IiBocmVmPSJtYWlsdG86YWRmc2cuYWRmc2RAZGZzZ2QtZmRzLmFmZCI+YWRmc2cuYWRmc2RAZGZzZ2QtZmRzLmFmZDwvYT4mbmJzcDtUeXBlIGJyYW5kLCBwcm9kdWN0IG5hbWUsIHByb2R1Y3QtY29kZSBhbmQgcHJvZHVjdCB0eXBlIGFzIGNvbXBsZXRlIGFzIHBvc3NpYmxlIHRvIG9idGFpbiBtb3JlIGFjY3VyYXRlIHJlc3VsdC48L2Rpdj4KPGRpdiBzdHlsZT0ibWFyZ2luOiAwcHg7IHBhZGRpbmc6IDBweDsgYm9yZGVyOiAwcHg7IGZvbnQtc2l6ZTogMTZweDsgdmVydGljYWwtYWxpZ246IGJhc2VsaW5lOyBjb2xvcjogIzMzMzMzMzsgZm9udC1mYW1pbHk6ICdTb3VyY2UgU2FucyBQcm8nLCBzYW5zLXNlcmlmOyI+VGhlcmUgaXMgMjE1LDY3NyBwcm9kdWN0IGxpc3RlZCBvbiBCbGlibGkuY29tLiBCeSB1c2luZyBkYXRhIGZyb20gc2ltaWxhciBwcm9kdWN0cyB0aGF0IGFscmVhZHkgZXhpc3QsIHlvdSBkbyBub3QgbmVlZCB0byByZWZpbGwgdGhlIGZlYXR1cmUgZGV0YWlscyBvZiB0aGUgcHJvZHVjdDwvZGl2Pgo8L2JvZHk+CjwvaHRtbD4=";

  private ProductDetailResponse productDetailResponse;
  private RestrictedKeywordsByField restrictedKeywordsByField;
  private List<RestrictedKeywordsByField> restrictedKeywordsByFieldList;

  private ProductDetailResponse getProductDetailResponse() {
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    productDetailResponse.setDescription(DEFAULT_PRODUCT_DESCRIPTION.getBytes());
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(DEFAULT_ATTRIBUTE_VALUE);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    return productDetailResponse;
  }

  @BeforeEach
  public void setup() {
    restrictedKeywordsByField = new RestrictedKeywordsByField();
    restrictedKeywordsByFieldList = new ArrayList<>();
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsValidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(NAME_WITH_VALID_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsInvalidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(NAME_WITH_INVALID_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsDescInValidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setDescription(DESCRIPTION_WITH_INVALID_PHONE_NUMBER.getBytes());
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsDescValidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setDescription(DESCRIPTION_WITH_VALID_PHONE_NUMBER.getBytes());
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsUSPValidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setUniqueSellingPoint(USP_WITH_VALID_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsUSPInvalidTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setUniqueSellingPoint(USP_WITH_INVALID_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsValidEmailTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(NAME_WITH_VALID_EMAIL);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsInvalidEmailTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(NAME_WITH_INVALID_EMAIL);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsAttributesTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(NAME_WITH_VALID_EMAIL);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsAttributesHyphenTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(Constants.DELIMITER_DASH);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsAttributesPhoneTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(NAME_WITH_VALID_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsAsValidWordNumberTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(VALID_WORD_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsAsInvalidWordNumberTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue(INVALID_WORD_PHONE_NUMBER);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isPhoneNumberPresentInProductDetailsMultipleAttributesTest() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue(INVALID_WORD_PHONE_NUMBER);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    List<ProductAttributeResponse> responses = new ArrayList<>(productDetailResponse.getProductAttributeResponses());
    responses.add(productAttributeResponse);
    productDetailResponse.setProductAttributeResponses(responses);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest1() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    byte[] descByteArray = Base64.decodeBase64(DESC_BYTES.getBytes());
    productDetailResponse.setDescription(descByteArray);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest2() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    byte[] descByteArray = Base64.decodeBase64(DESC_BYTES2.getBytes());
    productDetailResponse.setDescription(descByteArray);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest3() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    byte[] descByteArray = Base64.decodeBase64(DESC_BYTES3.getBytes());
    productDetailResponse.setDescription(descByteArray);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest4() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setUniqueSellingPoint(null);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest5() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setProductAttributeResponses(new ArrayList<>());
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest6() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.getProductAttributeResponses().get(0).setProductAttributeValues(new ArrayList<>());
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest7() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(VALID_WORD_PHONE_NUMBER1);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }

  @Test
  public void isEmailInProductDetailsDescValidTest8() {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productDetailResponse.setName(StringUtils.EMPTY);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, PHONE_REGEX,
        restrictedKeywordsByFieldList);
    Assertions.assertFalse(CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList));
  }
}
