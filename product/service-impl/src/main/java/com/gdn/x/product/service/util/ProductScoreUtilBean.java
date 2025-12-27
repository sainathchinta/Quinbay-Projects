package com.gdn.x.product.service.util;

import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.StringTokenizer;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ProductScoreRuleConstants;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.ItemScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;
import com.gdn.x.product.rest.web.model.response.IgnoreAttributeSet;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.RuleConfigResponse;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

@Service
public class ProductScoreUtilBean implements ProductScoreUtil {

  public static final double ROUND_OFF_FACTOR = 100D;
  public static final double DEFAULT_SCORE = 0.0;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;


  private static final String REGEX_FOR_HTML_TAGS = "\\<.*?\\>|&\\w+.;";
  private static final String REGEX_FOR_NON_ASCII_CHARS = "[^\\x00-\\x7F]";
  private static final String REGEX_FOR_SPECIAL_CHARS = "[^A-Za-z0-9]";
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_HTML_TAGS = Pattern.compile(REGEX_FOR_HTML_TAGS);
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS = Pattern.compile(REGEX_FOR_NON_ASCII_CHARS);
  private static final Pattern PATTERN_FOR_SPECIAL_CHARS = Pattern.compile(REGEX_FOR_SPECIAL_CHARS);
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);

  @Override
  public ProductScoreVo getProductScoreByProductScoreRequest(ProductScoreRequest productScoreRequest) throws Exception {
    ProductScoreRuleResponse productScoreRuleResponse =
        categoryService.getProductScoreRuleForCategory(productScoreRequest.getCategoryCode());
    CategoryDetailResponse categoryDetailResponse = productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID,
            ProductScoreRuleConstants.USERNAME, productScoreRequest.getCategoryCode());
    if (Objects.isNull(categoryDetailResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    }
    populateAttributeRequestFlags(productScoreRequest, categoryDetailResponse);
    ProductScoreVo productScore = new ProductScoreVo();
    productScoreRuleResponse.getProductScoreRules().keySet().forEach(ruleName -> {
      switch (ruleName) {
        case ProductScoreRuleConstants.MANDATORY_INFO: {
          setMandatoryScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.MANDATORY_INFO),
              productScore);
          break;
        }
        case ProductScoreRuleConstants.PRODUCT_TITLE_RULE: {
          setProductTitleScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.PRODUCT_TITLE_RULE),
              productScore, productScoreRuleResponse.getIgnoreSymbols());
          break;
        }
        case ProductScoreRuleConstants.DESCRIPTION_RULE: {
          setDescriptionScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.DESCRIPTION_RULE),
              productScore);
          break;
        }
        case ProductScoreRuleConstants.USP_RULE: {
          setUspScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.USP_RULE), productScore);
          break;
        }
        case ProductScoreRuleConstants.IMAGE_RULE: {
          if (!productScoreRequest.isSynchronised()) {
            setImageScore(productScoreRequest.getItemRequests(),
                productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.IMAGE_RULE), productScore);
          }
          break;
        }
        case ProductScoreRuleConstants.COMMON_IMAGE_RULE: {
          if (productScoreRequest.isSynchronised()) {
            int count = productScoreRequest.getItemRequests().stream()
                .flatMap(itemScoreRequest -> itemScoreRequest.getItemImages().stream())
                .filter(MasterDataProductImageDTO::isCommonImage)
                .collect(Collectors.groupingBy(MasterDataProductImageDTO::getLocationPath)).keySet().size();
            setCommonImageScore(count,
                productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.COMMON_IMAGE_RULE),
                productScore);
          }
          break;
        }
        case ProductScoreRuleConstants.SINGLE_VARIANT_IMAGE_RULE: {
          if (productScoreRequest.isSynchronised()) {
            if (productScoreRequest.getItemRequests().size() == 1) {
              setVariantImageScore(productScoreRequest.getItemRequests(),
                  productScoreRuleResponse.getProductScoreRules()
                      .get(ProductScoreRuleConstants.SINGLE_VARIANT_IMAGE_RULE), productScore);
            }
          }
          break;
        }
        case ProductScoreRuleConstants.MULTI_VARIANT_IMAGE_RULE: {
          if (productScoreRequest.isSynchronised()) {
            if (productScoreRequest.getItemRequests().size() > 1) {
              setVariantImageScore(productScoreRequest.getItemRequests(),
                  productScoreRuleResponse.getProductScoreRules()
                      .get(ProductScoreRuleConstants.MULTI_VARIANT_IMAGE_RULE), productScore);
            }
          }
          break;
        }
        case ProductScoreRuleConstants.VIDEO_URL: {
          setVideoUrlScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.VIDEO_URL),
              productScore);
          break;
        }
        case ProductScoreRuleConstants.VARIANT_CREATING_RULE: {
          setVariantCreationScore(productScoreRequest,
              productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.VARIANT_CREATING_RULE), categoryDetailResponse,
              productScore);
          break;
        }
        case ProductScoreRuleConstants.REMAINING_ATTRIBUTE_RULE: {
          setRemainingAttributesScore(productScoreRequest,
              productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.REMAINING_ATTRIBUTE_RULE), categoryDetailResponse,
              productScore);
          break;
        }
        case ProductScoreRuleConstants.RECOMMENDED_ATTRIBUTE_RULE: {
          setRecommendedAttributesScore(productScoreRequest.getProductAttributeRequests(),
              productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.RECOMMENDED_ATTRIBUTE_RULE),
              categoryDetailResponse, productScore, productScoreRuleResponse.getIgnoreAttributes());
          break;
        }
        case ProductScoreRuleConstants.EAN_UPC_RULE: {
          setEanUpcScore(productScoreRequest, productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.EAN_UPC_RULE), productScore);
          break;
        }
      }
    });
    productScore.setTotalScore(
        (productScore.getMandatoryAttributeScore() + productScore.getProductTitleScore() + productScore
            .getRecommendedAttributeScore() + productScore.getDescriptionScore() + productScore.getImageScore()
            + productScore.getRemainingAttributeScore() + productScore.getUspScore() + productScore
            .getVariantCreatingScore() + productScore.getVideoUrlScore() + productScore.getEanUpcScore())
            * ROUND_OFF_FACTOR / ROUND_OFF_FACTOR);
    productScore.setTotalScore(productScore.getTotalScore() * ROUND_OFF_FACTOR / ROUND_OFF_FACTOR);
    return productScore;
  }

  private void setCommonImageScore(int count, MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse,
      ProductScoreVo productScore) {
    for (RuleConfigResponse ruleConfig : maxScoreAndRuleConfigResponse.getRuleConfig()) {
      if (evaluate(ruleConfig.getOperator(), count, ruleConfig.getValue())) {
        productScore.setImageScore(productScore.getImageScore() + ruleConfig.getScore());
        break;
      }
    }
  }

  @Override
  public double getRecommendedAttributeScore(List<AttributeScoreRequest> attributeScoreRequests, String categoryCode)
      throws Exception {
    ProductScoreRuleResponse productScoreRuleResponse =
        categoryService.getProductScoreRuleForCategory(categoryCode);
    CategoryDetailResponse categoryDetailResponse = productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            categoryCode);
    if (Objects.isNull(categoryDetailResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    }
    ProductScoreRequest productScoreRequest = new ProductScoreRequest();
    productScoreRequest.setProductAttributeRequests(attributeScoreRequests);
    populateAttributeRequestFlags(productScoreRequest, categoryDetailResponse);
    return setRecommendedAttributesScore(productScoreRequest.getProductAttributeRequests(),
        productScoreRuleResponse.getProductScoreRules().get(ProductScoreRuleConstants.RECOMMENDED_ATTRIBUTE_RULE),
        categoryDetailResponse, new ProductScoreVo(), productScoreRuleResponse.getIgnoreAttributes());
  }

  private void populateAttributeRequestFlags(ProductScoreRequest productScoreRequest,
      CategoryDetailResponse categoryDetailResponse) {
    Map<String, CategoryAttributeResponse> attributeResponseMap =
        categoryDetailResponse.getCategoryAttributes().stream()
            .filter(categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete()).collect(Collectors
            .toMap(categoryAttributeResponse -> categoryAttributeResponse.getAttribute().getAttributeCode(),
                Function.identity()));
    productScoreRequest.setProductAttributeRequests(productScoreRequest.getProductAttributeRequests().stream().
        filter(attributeScoreRequest -> Objects
            .nonNull(getAttributeScoreRequest(attributeScoreRequest, attributeResponseMap)))
        .collect(Collectors.toList()));
  }

  private AttributeScoreRequest getAttributeScoreRequest(AttributeScoreRequest attributeScoreRequest, Map<String, CategoryAttributeResponse> attributeResponseMap) {
    if (attributeResponseMap.containsKey(attributeScoreRequest.getAttributeCode())) {
      CategoryAttributeResponse categoryAttributeResponse = attributeResponseMap.get(attributeScoreRequest.getAttributeCode());
      attributeScoreRequest.setAttributeType(categoryAttributeResponse.getAttribute().getAttributeType());
      attributeScoreRequest.setBasicView(categoryAttributeResponse.getAttribute().isBasicView());
      attributeScoreRequest.setMandatory(categoryAttributeResponse.getAttribute().isMandatory());
      attributeScoreRequest.setVariantCreation(categoryAttributeResponse.getAttribute().isVariantCreation());
      attributeScoreRequest.setSkuValue(categoryAttributeResponse.getAttribute().isSkuValue());
      attributeScoreRequest.setName(categoryAttributeResponse.getAttribute().getName());
      return attributeScoreRequest;
    }
    return null;
  }

  private void setEanUpcScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScoreVo) {
    if (CollectionUtils.isNotEmpty(productScoreRequest.getItemRequests()) && productScoreRequest.getItemRequests()
        .stream().allMatch(itemRequest -> StringUtils.isNotBlank(itemRequest.getUpcCode()) && isValidUPCCode(
            itemRequest.getUpcCode()))) {
      productScoreVo.setEanUpcScore(maxScoreAndRuleConfigResponse.getMaxScore());
    }
  }

  //Not adding category check in mandatory score because if category is null then rules API itself will fail.
  private void setMandatoryScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    //Giving category score as default score
    int eligibleForMandatoryScore = 1;
    if (StringUtils.isNotBlank(productScoreRequest.getBrand())) {
      eligibleForMandatoryScore++;
    }
    if (StringUtils.isNotBlank(productScoreRequest.getName()) && StringUtils
        .isNotBlank(PATTERN_FOR_SPECIAL_CHARS.matcher(productScoreRequest.getName()).replaceAll(StringUtils.EMPTY))) {
      eligibleForMandatoryScore++;
    }
    if (Objects.nonNull(productScoreRequest.getDescription()) && productScoreRequest.getDescription().length > 0
        && processAndValidateDescription(productScoreRequest.getDescription())) {
      eligibleForMandatoryScore++;
    }
    productScore.setMandatoryAttributeScore(
        (maxScoreAndRuleConfigResponse.getMaxScore() * eligibleForMandatoryScore * ROUND_OFF_FACTOR) / (4
            * ROUND_OFF_FACTOR));
  }

  private boolean processAndValidateDescription(byte[] description) {
    String descriptionString = new String(Base64.getDecoder().decode(Base64.getEncoder().encode(description)));
    String descriptionWithoutNonAscii =
        PATTERN_FOR_NON_ASCII_CHARS.matcher(descriptionString).replaceAll(StringUtils.SPACE);
    String descriptionWithoutHtmlTags =
        PATTERN_FOR_HTML_TAGS.matcher(descriptionWithoutNonAscii).replaceAll(StringUtils.EMPTY);
    if (StringUtils
        .isNotBlank(PATTERN_FOR_SPECIAL_CHARS.matcher(descriptionWithoutHtmlTags).replaceAll(StringUtils.EMPTY))) {
      return true;
    }
    return false;
  }

  private void setProductTitleScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse productScoreRuleDto, ProductScoreVo productScore, List<String> ignoreSymbols) {
    if (StringUtils.isNotBlank(productScoreRequest.getName()) && StringUtils
        .isNotBlank(productScoreRequest.getBrand())) {
      if (CollectionUtils.isNotEmpty(ignoreSymbols)) {
        ignoreSymbols.forEach(
            symbol -> productScoreRequest.setName(productScoreRequest.getName().replace(symbol, StringUtils.EMPTY)));
        ignoreSymbols.forEach(
            symbol -> productScoreRequest.setBrand(productScoreRequest.getBrand().replace(symbol, StringUtils.EMPTY)));
      }
      if (productScoreRequest.getName().toLowerCase().contains(productScoreRequest.getBrand().toLowerCase())) {
        productScore.setProductTitleScore(productScoreRuleDto.getMaxScore());
      }
    }
  }

  private void setDescriptionScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    if (Objects.nonNull(productScoreRequest.getDescription())) {
      String description =
          new String(Base64.getDecoder().decode(Base64.getEncoder().encode(productScoreRequest.getDescription())));
      productScore.setDescriptionScore(getDescriptionScore(description, maxScoreAndRuleConfigResponse));
    }
  }

  private void setUspScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    String uniqueSellingPoint = productScoreRequest.getUniqueSellingPoint();
    productScore.setUspScore(getUspScore(uniqueSellingPoint, maxScoreAndRuleConfigResponse));
  }

  private void setImageScore(List<ItemScoreRequest> itemScoreRequests,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    double imageScore = DEFAULT_SCORE;
    for (ItemScoreRequest itemScoreRequest : itemScoreRequests) {
      double itemImageScore = DEFAULT_SCORE;
      for (RuleConfigResponse ruleConfig : maxScoreAndRuleConfigResponse.getRuleConfig()) {
        if (evaluate(ruleConfig.getOperator(), itemScoreRequest.getItemImages().size(), ruleConfig.getValue())
            && itemImageScore < (ruleConfig.getScore() / itemScoreRequests.size())) {
          itemImageScore = (ruleConfig.getScore() * ROUND_OFF_FACTOR / itemScoreRequests.size()) / ROUND_OFF_FACTOR;
        }
      }
      imageScore += itemImageScore;
    }
    productScore.setImageScore(imageScore);
  }

  private void setVariantImageScore(List<ItemScoreRequest> itemScoreRequests,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    double imageScore = DEFAULT_SCORE;
    for (ItemScoreRequest itemScoreRequest : itemScoreRequests) {
      double itemImageScore = DEFAULT_SCORE;
      for (RuleConfigResponse ruleConfig : maxScoreAndRuleConfigResponse.getRuleConfig()) {
        if (!itemScoreRequestForVariant(itemScoreRequest).isEmpty() && evaluate(ruleConfig.getOperator(),
            itemScoreRequestForVariant(itemScoreRequest).size(), ruleConfig.getValue()) && itemImageScore < (
            ruleConfig.getScore()
            / itemScoreRequests.size())) {
          itemImageScore = (ruleConfig.getScore() * ROUND_OFF_FACTOR / itemScoreRequests.size()) / ROUND_OFF_FACTOR;
          break;
        }
      }
      imageScore += itemImageScore;
    }
    productScore.setImageScore(productScore.getImageScore() + imageScore);
  }

  private static List<MasterDataProductImageDTO> itemScoreRequestForVariant(ItemScoreRequest itemScoreRequest) {
    return itemScoreRequest.getItemImages().stream()
        .filter(masterDataProductImageDTO -> !masterDataProductImageDTO.isCommonImage()).collect(Collectors.toList());
  }

  private void setVideoUrlScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, ProductScoreVo productScore) {
    if (StringUtils.isNotBlank(productScoreRequest.getUrl())) {
      productScore.setVideoUrlScore(maxScoreAndRuleConfigResponse.getMaxScore());
    }
  }

  private void setVariantCreationScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, CategoryDetailResponse categoryDetailResponse,
      ProductScoreVo productScore) {
    int definingAttributeCount = categoryDetailResponse.getCategoryAttributes().stream()
        .filter(categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete()).filter(
            categoryAttributeResponse -> isDefiningAttribute(
                categoryAttributeResponse.getAttribute().getAttributeType(),
                categoryAttributeResponse.getAttribute().isVariantCreation())).collect(Collectors.toList()).size();
    // Give max score for zero defining attributes or when atleast one defining attribute is filled
    if (definingAttributeCount == 0 || productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> isDefiningAttribute(attributeScoreRequest.getAttributeType(),
            attributeScoreRequest.isVariantCreation()))
        .anyMatch(attributeScoreRequest -> CollectionUtils.isNotEmpty(attributeScoreRequest.getValues()))) {
      productScore.setVariantCreatingScore(maxScoreAndRuleConfigResponse.getMaxScore());
    }
  }

  private void setRemainingAttributesScore(ProductScoreRequest productScoreRequest,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, CategoryDetailResponse categoryDetailResponse,
      ProductScoreVo productScore) {
    //Filter out basic view false which are not sku value true (Exclude defining attributes , brand and family colour)
    Map<String, CategoryAttributeResponse> advancedViewAttributes = categoryDetailResponse.getCategoryAttributes().stream().filter(
        categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete() && !isDefiningAttribute(
            categoryAttributeResponse.getAttribute().getAttributeType(),
            categoryAttributeResponse.getAttribute().isVariantCreation()) && !categoryAttributeResponse.getAttribute()
            .isBasicView() && Boolean.FALSE.equals(categoryAttributeResponse.getAttribute().isSkuValue())
            && !filterBrandAndFamilyColourAttributes(categoryAttributeResponse.getAttribute().getName())).collect(
        Collectors.toMap(categoryAttributeResponse -> categoryAttributeResponse.getAttribute().getAttributeCode(),
            Function.identity()));
    int eligibleForScoring = 0;
    if (advancedViewAttributes.size() != 0) {
      //Filter out basic view false which are not sku value true (Exclude defining attributes , brand and family colour)
      eligibleForScoring = productScoreRequest.getProductAttributeRequests().stream().filter(attributeScoreRequest ->
          !isDefiningAttribute(attributeScoreRequest.getAttributeType(), attributeScoreRequest.isVariantCreation())
              && !attributeScoreRequest.isBasicView() && Boolean.FALSE.equals(attributeScoreRequest.getSkuValue())
              && !filterBrandAndFamilyColourAttributes(attributeScoreRequest.getName()) && (
              CollectionUtils.isNotEmpty(attributeScoreRequest.getValues()) && StringUtils.isNotBlank(
                  PATTERN_FOR_SPECIAL_CHARS.matcher(attributeScoreRequest.getValues().get(0))
                      .replaceAll(StringUtils.EMPTY)))).collect(Collectors.toList()).size();
      double remainingAttributeScore = Math.round(
          (maxScoreAndRuleConfigResponse.getMaxScore() * eligibleForScoring * ROUND_OFF_FACTOR) / advancedViewAttributes
              .size())
              / ROUND_OFF_FACTOR;
      productScore.setRemainingAttributeScore(remainingAttributeScore);
    } else {
      productScore.setRemainingAttributeScore(maxScoreAndRuleConfigResponse.getMaxScore());
    }
  }

  public static boolean isValidUPCCode(String upcCode) {
    try {
      Long.parseLong(upcCode);
    } catch (NumberFormatException e) {
      return false;
    }
    int length = upcCode.length();
    if (!(length == 5 || length == 8 || length == 12 || length == 13)) {
      return false;
    }
    return true;
  }

  private boolean filterBrandAndFamilyColourAttributes(String attributeName) {
    return (ProductScoreRuleConstants.BRAND.equalsIgnoreCase(attributeName) || ProductScoreRuleConstants.FAMILY_COLOUR
        .equalsIgnoreCase(attributeName));
  }

  // This will be called on sync product update flow
  public double setRecommendedAttributesScore(List<AttributeScoreRequest> attributeScoreRequests,
      MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse, CategoryDetailResponse categoryDetailResponse,
      ProductScoreVo productScore, List<IgnoreAttributeSet> ignoreAttributeSets) {
    //Filter out sku value true or basic view true attributes (Exclude defining attributes , brand and family colour)
    Map<String, CategoryAttributeResponse> recommendedAttributes =
        categoryDetailResponse.getCategoryAttributes().stream().filter(
            categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete() && !isDefiningAttribute(
                categoryAttributeResponse.getAttribute().getAttributeType(),
                categoryAttributeResponse.getAttribute().isVariantCreation()) && !filterBrandAndFamilyColourAttributes(
                categoryAttributeResponse.getAttribute().getName()) && (
                categoryAttributeResponse.getAttribute().isBasicView() || categoryAttributeResponse.getAttribute()
                    .isSkuValue())).collect(Collectors
            .toMap(categoryAttributeResponse -> categoryAttributeResponse.getAttribute().getAttributeCode(),
                Function.identity()));
    if (recommendedAttributes.size() != 0) {
      processIgnoreAttributesSet(recommendedAttributes, attributeScoreRequests, ignoreAttributeSets);
      double recommendedAttributeScore = Math.round(
          maxScoreAndRuleConfigResponse.getMaxScore() * getEligibleRecommendedAttributesCount(attributeScoreRequests,
              recommendedAttributes) * ROUND_OFF_FACTOR / recommendedAttributes.size()) / ROUND_OFF_FACTOR;
      productScore.setRecommendedAttributeScore(recommendedAttributeScore);
    } else {
      productScore.setRecommendedAttributeScore(maxScoreAndRuleConfigResponse.getMaxScore());
    }
    return productScore.getRecommendedAttributeScore();
  }

  private void processIgnoreAttributesSet(Map<String, CategoryAttributeResponse> recommendedAttributes,
      List<AttributeScoreRequest> attributeScoreRequests, List<IgnoreAttributeSet> ignoreAttributeSets) {
    for (IgnoreAttributeSet ignoreAttributeSet : ignoreAttributeSets) {
      if (attributeScoreRequests.stream().anyMatch(attributeScoreRequest -> (
          attributeScoreRequest.getName().equalsIgnoreCase(ignoreAttributeSet.getAttributeName()) && CollectionUtils
              .isNotEmpty(attributeScoreRequest.getValues()) && attributeScoreRequest.getValues().get(0)
              .equalsIgnoreCase(ignoreAttributeSet.getValue())))) {
        List<String> invalidAttributes = ignoreAttributeSet.getIgnoreAttributeNames().stream()
            .map(attributeName -> getAttributeCodeByAttributeName(recommendedAttributes, attributeName))
            .filter(StringUtils::isNotBlank).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(invalidAttributes)) {
          invalidAttributes.forEach(attribute -> recommendedAttributes.remove(attribute));
        }
        ignoreAttributeSet.getIgnoreAttributeNames().forEach(attributeName -> attributeScoreRequests
            .removeIf(attributeScoreRequest -> attributeName.equalsIgnoreCase(attributeScoreRequest.getName())));
      }
    }
  }

  private String getAttributeCodeByAttributeName(Map<String, CategoryAttributeResponse> recommendedAttributes,
      String attributeName) {
    Optional<CategoryAttributeResponse> attributeResponse = Optional.ofNullable(recommendedAttributes.values().stream().filter(
        categoryAttributeResponse -> attributeName.equalsIgnoreCase(categoryAttributeResponse.getAttribute().getName()))
        .findFirst().orElse(null));
    return Optional.empty().equals(attributeResponse) ?
        StringUtils.EMPTY :
        attributeResponse.get().getAttribute().getAttributeCode();
  }

  private int getEligibleRecommendedAttributesCount(List<AttributeScoreRequest> attributeScores,
      Map<String, CategoryAttributeResponse> categoryAttributeResponseMap) {
    //Filter out sku value true or basic view true attributes (Exclude defining attributes , brand and family colour)
    List<AttributeScoreRequest> attributeScoreRequests = attributeScores.stream().filter(attributeScoreRequest ->
        !isDefiningAttribute(attributeScoreRequest.getAttributeType(), attributeScoreRequest.isVariantCreation())
            && !filterBrandAndFamilyColourAttributes(attributeScoreRequest.getName()) && (
            attributeScoreRequest.isBasicView() || Boolean.TRUE.equals(attributeScoreRequest.getSkuValue())))
        .collect(Collectors.toList());
    int eligibleForScoring = 0;
    for (AttributeScoreRequest attributeScoreRequest : attributeScoreRequests) {
      if (CollectionUtils.isNotEmpty(attributeScoreRequest.getValues()) && StringUtils
          .isNotBlank(attributeScoreRequest.getValues().get(0)) && StringUtils.isNotBlank(
          PATTERN_FOR_SPECIAL_CHARS.matcher(attributeScoreRequest.getValues().get(0)).replaceAll(StringUtils.EMPTY))) {
        eligibleForScoring++;
      }
    }
    return eligibleForScoring;
  }

  private boolean isDefiningAttribute(String attributeType, boolean variantCreation) {
    if (AttributeType.DEFINING_ATTRIBUTE.name().equalsIgnoreCase(attributeType) || variantCreation) {
      return true;
    }
    return false;
  }

  private double getDescriptionScore(String description, MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse) {
    double score = DEFAULT_SCORE;
    String descriptionWithoutNonAscii =
        PATTERN_FOR_NON_ASCII_CHARS.matcher(description).replaceAll(StringUtils.SPACE);
    String descriptionWithoutHtmlTags =
        PATTERN_FOR_HTML_TAGS.matcher(descriptionWithoutNonAscii).replaceAll(StringUtils.EMPTY);
    if (StringUtils.isNotBlank(PATTERN_FOR_SPECIAL_CHARS.matcher(descriptionWithoutHtmlTags).replaceAll(StringUtils.EMPTY))) {
      int count = new StringTokenizer(descriptionWithoutHtmlTags).countTokens();
      for (RuleConfigResponse ruleConfig : maxScoreAndRuleConfigResponse.getRuleConfig()) {
        if (evaluate(ruleConfig.getOperator(), count, ruleConfig.getValue()) && score <= ruleConfig.getScore()) {
          score = ruleConfig.getScore();
        }
      }
    }
    return score;
  }

  private double getUspScore(String usp, MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse) {
    double score = DEFAULT_SCORE;
    if (StringUtils.isNotBlank(usp) && StringUtils.isNotBlank(
      PATTERN_FOR_SPECIAL_CHARS.matcher(usp).replaceAll(StringUtils.EMPTY))) {
      String uspWithoutNonAscii = PATTERN_FOR_NON_ASCII_CHARS.matcher(usp).replaceAll(StringUtils.SPACE);
      String uspWithoutHtmlTags = PATTERN_FOR_HTML_TAGS.matcher(uspWithoutNonAscii).replaceAll(StringUtils.EMPTY);
      String uspAfterTrimming =
          PATTERN_FOR_EXTRA_SPACE.matcher(uspWithoutHtmlTags.trim()).replaceAll(StringUtils.SPACE);
      for (RuleConfigResponse ruleConfig : maxScoreAndRuleConfigResponse.getRuleConfig()) {
        if (evaluate(ruleConfig.getOperator(), uspAfterTrimming.length(), ruleConfig.getValue()) && score <= ruleConfig
            .getScore()) {
          score = ruleConfig.getScore();
        }
      }
    }
    return score;
  }

  private boolean evaluate(String operator, int firstValue, int secondValue) {
    switch (operator) {
      case ProductScoreRuleConstants.GREATER_THAN_OR_EQUAL_TO:
        return firstValue >= secondValue;

      case ProductScoreRuleConstants.LESS_THAN_OR_EQUAL_TO:
        return firstValue <= secondValue;

      case ProductScoreRuleConstants.LESS_THAN:
        return firstValue < secondValue;

      case ProductScoreRuleConstants.GREATER_THAN:
        return firstValue > secondValue;

      case ProductScoreRuleConstants.EQUAL_TO:
        return firstValue == secondValue;

      case ProductScoreRuleConstants.NOT_EQUALS_TO:
        return firstValue != secondValue;

      default:
        return false;
    }
  }
}