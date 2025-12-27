package com.gdn.mta.product.util.validator;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class RestrictedKeywordValidator {
  private static final String REGEX_META_CHARACTERS = "[`~!@#$%^&*()_+\\[\\]\\\\;\',/{}|:\"<>?]";

  public static List<RestrictedKeywordsByField> getRestrictedKeywordPresentInProductDetail(
      List<String> restrictedKeywordMappedToCategory, ProductDetailResponse productDetailResponse) {
    Pattern pattern = Pattern
        .compile("\\b(" + StringUtils.join(escapeMetaCharacters(restrictedKeywordMappedToCategory), "|") + ")\\b",
            Pattern.CASE_INSENSITIVE);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
    RestrictedKeywordsByField productNameRestrictedKeywords =
        extractRestrictedKeywordUsingPattern(productDetailResponse.getName(), pattern,
            RestrictedKeywordFieldNames.PRODUCT_NAME);
    RestrictedKeywordsByField uspRestrictedKeywords =
        extractRestrictedKeywordUsingPattern(productDetailResponse.getUniqueSellingPoint(), pattern,
            RestrictedKeywordFieldNames.USP);
    RestrictedKeywordsByField descriptionRestrictedKeywords =
        extractRestrictedKeywordUsingPattern(new String(productDetailResponse.getDescription(), StandardCharsets.UTF_8),
            pattern, RestrictedKeywordFieldNames.DESCRIPTION);
    addToRestrictedKeywordList(productNameRestrictedKeywords, restrictedKeywordsByFieldList);
    addToRestrictedKeywordList(uspRestrictedKeywords, restrictedKeywordsByFieldList);
    addToRestrictedKeywordList(descriptionRestrictedKeywords, restrictedKeywordsByFieldList);
    return restrictedKeywordsByFieldList;
  }

  private static RestrictedKeywordsByField extractRestrictedKeywordUsingPattern(String text, Pattern pattern,
      RestrictedKeywordFieldNames restrictedKeywordFieldNames) {
    RestrictedKeywordsByField restrictedKeywordsByField =
        new RestrictedKeywordsByField(restrictedKeywordFieldNames.name(), new ArrayList<>());
    Matcher matcher = pattern.matcher(text);
    while (matcher.find()) {
      String keyword = matcher.group();
      if (!restrictedKeywordsByField.getKeywords().contains(keyword)) {
        restrictedKeywordsByField.getKeywords().add(keyword);
      }
    }
    return restrictedKeywordsByField;
  }

  private static void addToRestrictedKeywordList(RestrictedKeywordsByField restrictedKeywordsByField,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList) {
    if (CollectionUtils.isNotEmpty(restrictedKeywordsByField.getKeywords())) {
      restrictedKeywordsByFieldList.add(restrictedKeywordsByField);
    }
  }

  private static List<String> escapeMetaCharacters(List<String> restrictedKeywordMappedToCategory) {
    return restrictedKeywordMappedToCategory.stream()
        .map(keyword -> keyword.replaceAll(REGEX_META_CHARACTERS, StringUtils.EMPTY).trim())
        .collect(Collectors.toList());
  }

}
