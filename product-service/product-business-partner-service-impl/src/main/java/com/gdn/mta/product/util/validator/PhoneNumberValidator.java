package com.gdn.mta.product.util.validator;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class PhoneNumberValidator {

  private static final String REGEX_FOR_NUMBERS = "[^0-9]";
  private static final String REGEX_FOR_ALPHANUMERICS = "[^a-zA-Z0-9]";

  private static HashMap<String, Integer> numberMap;

  static {
    numberMap = new HashMap<>();
    numberMap.put("zero", 0);
    numberMap.put("one", 1);
    numberMap.put("two", 2);
    numberMap.put("three", 3);
    numberMap.put("four", 4);
    numberMap.put("five", 5);
    numberMap.put("six", 6);
    numberMap.put("seven", 7);
    numberMap.put("eight", 8);
    numberMap.put("nine", 9);
  }

  public static void isPhoneNumberOrEmailPresentInProductDetails(ProductDetailResponse productDetailResponse,
      String phoneNumberDetectionRegex, List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    Map<String, RestrictedKeywordsByField> stringRestrictedKeywordsDetectedMap =
        restrictedKeywordsByFieldList.stream().collect(Collectors
            .toMap(restrictedKeywordsDetected -> restrictedKeywordsDetected.getFieldIdentifier(),
                restrictedKeywordsDetected -> restrictedKeywordsDetected, (oldObject, newObject) -> newObject));
    isPhoneNumberOrEmailRestrictedKeywordPresent(productDetailResponse.getName(), phoneNumberDetectionRegex,
        restrictedKeywordsByFieldList, stringRestrictedKeywordsDetectedMap, RestrictedKeywordFieldNames.PRODUCT_NAME);
    isPhoneNumberOrEmailRestrictedKeywordPresent(productDetailResponse.getUniqueSellingPoint(), phoneNumberDetectionRegex,
        restrictedKeywordsByFieldList, stringRestrictedKeywordsDetectedMap, RestrictedKeywordFieldNames.USP);
    isPhoneNumberOrEmailRestrictedKeywordPresent(new String(productDetailResponse.getDescription(), StandardCharsets.UTF_8), phoneNumberDetectionRegex,
        restrictedKeywordsByFieldList, stringRestrictedKeywordsDetectedMap, RestrictedKeywordFieldNames.DESCRIPTION);
    PhoneNumberValidator
        .checkPhoneNumberOrEmailInDescriptiveAttributes(productDetailResponse, phoneNumberDetectionRegex,
            restrictedKeywordsByFieldList);
  }

  private static void isPhoneNumberOrEmailRestrictedKeywordPresent(String data, String regex,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      Map<String, RestrictedKeywordsByField> restrictedKeywordsDetectedMap,
      RestrictedKeywordFieldNames restrictedKeywordFieldNames) {
    RestrictedKeywordsByField restrictedKeywordsByField =
        restrictedKeywordsDetectedMap.get(restrictedKeywordFieldNames.name());
    if (Objects.isNull(restrictedKeywordsByField)) {
      restrictedKeywordsByField =
          new RestrictedKeywordsByField(restrictedKeywordFieldNames.name(), new ArrayList<>());
      PhoneNumberValidator.checkPhoneNumberOrEmailInText(data, regex, restrictedKeywordsByField);
      if (CollectionUtils.isNotEmpty(restrictedKeywordsByField.getKeywords())) {
        restrictedKeywordsByFieldList.add(restrictedKeywordsByField);
      }
    } else {
      PhoneNumberValidator.checkPhoneNumberOrEmailInText(data, regex, restrictedKeywordsByField);
    }
  }

  private static void checkPhoneNumberOrEmailInDescriptiveAttributes(ProductDetailResponse productDetailResponse,
      String phoneNumberDetectionRegex, List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      productDetailResponse.getProductAttributeResponses().stream().filter(
          productAttributeResponse -> AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
              .equalsIgnoreCase(productAttributeResponse.getAttribute().getAttributeType())).forEach(
          productAttributeResponse -> checkRestrictedKeywordsInDescriptiveAttribute(productAttributeResponse,
              phoneNumberDetectionRegex, restrictedKeywordsByFieldList));
    }
  }

  private static void checkRestrictedKeywordsInDescriptiveAttribute(ProductAttributeResponse productAttributeResponse,
      String phoneNumberDetectionRegex, List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    RestrictedKeywordsByField descriptiveAttributeRestrictedKeywords =
        new RestrictedKeywordsByField(productAttributeResponse.getAttribute().getAttributeCode(), new ArrayList<>());
    checkPhoneNumberOrEmailInText(productAttributeResponse.getProductAttributeValues(), phoneNumberDetectionRegex,
        descriptiveAttributeRestrictedKeywords);
    if (CollectionUtils.isNotEmpty(descriptiveAttributeRestrictedKeywords.getKeywords())) {
      restrictedKeywordsByFieldList.add(descriptiveAttributeRestrictedKeywords);
    }
  }

  private static void checkPhoneNumberOrEmailInText(List<ProductAttributeValueResponse> valueResponses,
      String phoneNumberDetectionRegex, RestrictedKeywordsByField restrictedKeywordsByField) {
    if (CollectionUtils.isNotEmpty(valueResponses)) {
      valueResponses.forEach(productAttributeValueResponse -> PhoneNumberValidator
          .checkPhoneNumberOrEmailInText(productAttributeValueResponse.getDescriptiveAttributeValue(),
              phoneNumberDetectionRegex, restrictedKeywordsByField));
    }
  }

  private static void checkPhoneNumberOrEmailInText(String text, String phoneNumberDetectionRegex,
      RestrictedKeywordsByField restrictedKeywordsByField) {
    if (StringUtils.isNotBlank(text)) {
      Pattern mobileNumberPattern = Pattern.compile(phoneNumberDetectionRegex);
      text = text.toLowerCase();
      for (Map.Entry<String, Integer> entry : numberMap.entrySet()) {
        String keyword = entry.getKey();
        Integer value = entry.getValue();
        if (text.contains(keyword)) {
          text = text.replaceAll(keyword, String.valueOf(value));
        }
      }
      String formattedText = text.replaceAll(REGEX_FOR_ALPHANUMERICS, StringUtils.EMPTY);
      List<String> splittedFormattedText = new ArrayList<>();
      Matcher matcher = Pattern.compile("\\d+").matcher(formattedText);
      while (matcher.find()) {
        splittedFormattedText.add(matcher.group());
      }
      for (String splittedText : splittedFormattedText) {
        extractRestrictedKeywordUsingPattern(splittedText.replaceAll(REGEX_FOR_NUMBERS, StringUtils.EMPTY),
            mobileNumberPattern, restrictedKeywordsByField);
      }
      extractRestrictedKeywordUsingPattern(text.replaceAll(REGEX_FOR_NUMBERS, StringUtils.EMPTY), mobileNumberPattern,
          restrictedKeywordsByField);
      text = text.replaceAll("\\<.*?\\>", " ");
      String[] words = text.trim().split("\\s");
      for (String word : words) {
        EmailValidator.getEmailAddressFromProductFields(word, restrictedKeywordsByField);
        word = word.replaceAll(REGEX_FOR_NUMBERS, StringUtils.EMPTY);
        extractRestrictedKeywordUsingPattern(word, mobileNumberPattern, restrictedKeywordsByField);
      }
    }
  }

  private static void extractRestrictedKeywordUsingPattern(String data, Pattern pattern,
      RestrictedKeywordsByField restrictedKeywordsByField) {
    Matcher matcher = pattern.matcher(data);
    while (matcher.find()) {
      String keyword = matcher.group();
      if (!restrictedKeywordsByField.getKeywords().contains(keyword)) {
        restrictedKeywordsByField.getKeywords().add(keyword);
      }
    }
  }
}
