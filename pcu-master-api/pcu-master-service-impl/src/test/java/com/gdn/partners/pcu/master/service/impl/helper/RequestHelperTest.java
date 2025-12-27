package com.gdn.partners.pcu.master.service.impl.helper;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Arrays;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gdn.partners.pcu.master.model.request.MinWholesaleDiscountServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleConfigServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleMappingServiceRequest;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.master.web.model.request.AutoApprovalRuleDetailsDto;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import org.apache.commons.lang3.StringUtils;

import com.gdn.partners.pcu.master.model.attribute.AttributeValueUpdateModel;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.pcu.master.model.ErrorMessages;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class RequestHelperTest {

  private static final String ATTRIBUTE_VALUE_UPDATE_ID_1 = "id1";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_1 = "value1";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_1 = "code2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1 = 1;
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_2 = "value2";
  private static final Integer ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2 = 2;
  private static final String ATTRIBUTE_VALUE_UPDATE_ID_3 = "id3";
  private static final String ATTRIBUTE_VALUE_UPDATE_VALUE_3 = "value3";
  private static final String ATTRIBUTE_VALUE_UPDATE_CODE_3 = "code3";
  private static final String USER_NAME = "userName";
  private static final String DEFAULT_CATEGORY_CODE = "CGC-001";
  private static final int SEQUENCE = 1;
  private static final String KEYWORD = "KEYWORD";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String REQUEST = "REQUEST";
  private static final String INVALID_REQUEST = "<img src=x onerror=alert(document.domain)>";
  private static final String ENCODED_HTML = "&lt;h1&gt;Price Info&lt;/h1&gt;";
  private static final String ENCODED_GT = "&gt;";
  private static final String ENCODED_AMP = "&amp;";
  private static final String CONFIGURATION_TYPE = "configurationType";
  private static final Integer QUANTITY = 1;
  private static final Double PRICE = 2.0d;
  private MinWholesaleDiscountServiceRequest minWholesaleDiscountServiceRequest =
      MinWholesaleDiscountServiceRequest.builder().price(PRICE).build();
  private WholesaleConfigServiceRequest wholesaleConfigServiceRequest =
      WholesaleConfigServiceRequest.builder().quantity(QUANTITY)
          .minWholesaleDiscount(Collections.singletonList(minWholesaleDiscountServiceRequest))
          .build();
  private WholesaleMappingServiceRequest wholesaleMappingServiceRequest =
      WholesaleMappingServiceRequest.builder().configurationType(CONFIGURATION_TYPE).wholesaleConfig(
          Collections.singletonList(wholesaleConfigServiceRequest)).build();

  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto =
      AutoApprovalRuleDetailsDto.builder().build();
  private AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest =
      AutoQcConfigUpdateWebRequest.builder().ruleEnabled(true)
          .imageConfig(Collections.singletonList(autoApprovalRuleDetailsDto)).build();

  @Test
  void toMasterAttributeUpdateRequestTest() {
    AttributeValueUpdateModel attributeValueUpdateModel1 =
        AttributeValueUpdateModel.builder().id(ATTRIBUTE_VALUE_UPDATE_ID_1)
            .allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_1).value(ATTRIBUTE_VALUE_UPDATE_VALUE_1)
            .sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_1).build();
    AttributeValueUpdateModel attributeValueUpdateModel2 =
        AttributeValueUpdateModel.builder().value(ATTRIBUTE_VALUE_UPDATE_VALUE_2)
            .sequence(ATTRIBUTE_VALUE_UPDATE_SEQUENCE_2).build();
    AttributeValueUpdateModel attributeValueUpdateModel3 =
        AttributeValueUpdateModel.builder().id(ATTRIBUTE_VALUE_UPDATE_ID_3)
            .allowedAttributeCode(ATTRIBUTE_VALUE_UPDATE_CODE_3).value(ATTRIBUTE_VALUE_UPDATE_VALUE_3).build();
    AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest =
        AttributeValuesUpdateServiceRequest.builder().sortType(AttributeSortTypeRequest.MANUAL.toString())
            .attributeValues(Collections.singletonList(attributeValueUpdateModel1))
            .addedAttributeValues(Collections.singletonList(attributeValueUpdateModel2))
            .deletedAttributeValues(Collections.singletonList(attributeValueUpdateModel3)).updatedBy(USER_NAME).build();
    MasterAttributeUpdateRequest masterAttributeUpdateRequest =
        RequestHelper.toMasterAttributeUpdateRequest(attributeValuesUpdateServiceRequest);
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_1, masterAttributeUpdateRequest.getAttributeValues().get(0).getId());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_VALUE_2,
        masterAttributeUpdateRequest.getAddedAttributeValues().get(0).getValue());
    assertEquals(ATTRIBUTE_VALUE_UPDATE_ID_3, masterAttributeUpdateRequest.getDeletedAttributeValues().get(0).getId());
    assertEquals(AttributeSortTypeRequest.MANUAL, masterAttributeUpdateRequest.getSortType());
    assertEquals(USER_NAME, masterAttributeUpdateRequest.getUpdatedBy());
  }

  @Test
  void toCategoryInfoUpdateRequestForStatusTest() {
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = new CategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCategoryCode(DEFAULT_CATEGORY_CODE);
    RequestHelper.toCategoryInfoUpdateRequestForStatus(categoryStatusChangeServiceRequest);
  }

  @Test
  void toMasterAttributeAddRequestTest() {
    Date date = new Date();
    AttributeValueAddServiceRequest attributeValueAddServiceRequest =
        AttributeValueAddServiceRequest.builder().value(ATTRIBUTE_VALUE_UPDATE_VALUE_1).createdBy(USER_NAME)
            .createdDate(date).sequence(SEQUENCE).build();
    MasterAttributeAddRequest masterAttributeAddRequest =
        RequestHelper.toMasterAttributeAddRequest(attributeValueAddServiceRequest);
    assertEquals(ATTRIBUTE_VALUE_UPDATE_VALUE_1, masterAttributeAddRequest.getValue());
    assertEquals(USER_NAME, masterAttributeAddRequest.getCreatedBy());
    assertEquals(date, masterAttributeAddRequest.getCreatedDate());
    assertEquals(SEQUENCE, masterAttributeAddRequest.getSequence());
  }

  @Test
  void totoCategoryRestrictedKeywordsRequestTest() {
    CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest =
        CategoryRestrictedKeywordsWebRequest.builder().keyword(KEYWORD).categoryCode(CATEGORY_CODE).build();
    CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest =
        RequestHelper.toCategoryRestrictedKeywordsRequest(categoryRestrictedKeywordsWebRequest);
    assertEquals(CATEGORY_CODE, categoryRestrictedKeywordsRequest.getCategoryCode());
    assertEquals(KEYWORD, categoryRestrictedKeywordsRequest.getKeyword());
  }

  @Test
  void validateDataForHtml_switchOff() {
    String actualResponse = RequestHelper.validateDataForHtml(REQUEST, false);
    Assertions.assertEquals(REQUEST, actualResponse);
  }

  @Test
  void validateDataForHtml_emptyRequest() {
    String actualResponse = RequestHelper.validateDataForHtml(StringUtils.EMPTY, true);
    Assertions.assertEquals(StringUtils.EMPTY, actualResponse);
  }

  @Test
  void validateDataForHtml_encoded() {
    String actualResponse = RequestHelper.validateDataForHtml("<<a>img src=x onerror=alert('non-canary')>", true);
    Assertions.assertEquals("a>img src=x onerror=alert('non-canary')", actualResponse);
  }

  @Test
  void validateDataForHtml_switchOnEncodedHTML() {
    String actualResponse = RequestHelper.validateDataForHtml(ENCODED_HTML, true);
    Assertions.assertEquals("<h1>Price Info</h1>", actualResponse);
  }

  @Test
  void validateDataForHtml_switchOnEncodedGT() {
    String actualResponse = RequestHelper.validateDataForHtml(ENCODED_GT, true);
    Assertions.assertEquals(">", actualResponse);
  }

  @Test
  void validateDataForHtml_switchOnEncodedAMP() {
    String actualResponse = RequestHelper.validateDataForHtml(ENCODED_AMP, true);
    Assertions.assertEquals("&", actualResponse);
  }

  @Test
  void validateDataForHtml_switchOnHTML() {
    String actualResponse = RequestHelper.validateDataForHtml(INVALID_REQUEST, true);
    Assertions.assertEquals("img src=x onerror=alert(document.domain)", actualResponse);
  }

  @Test
  void checkArgumentTest() {
    RequestHelper.checkArgument(true, StringUtils.EMPTY);
  }

  @Test
  void checkArgumentExcetpionTest() {
    try {
      RequestHelper.checkArgument(false, "errorMessage");
    } catch (ValidationException e) {
      Assertions.assertEquals("errorMessage", e.getMessage());
    }
  }

  @Test
  void toWholeMappingRequest() {
    WholesaleMappingRequest wholesaleMappingRequest = new WholesaleMappingRequest();
    RequestHelper.toWholeMappingRequest(wholesaleMappingServiceRequest, wholesaleMappingRequest);
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesaleMappingRequest.getConfigurationType());
    Assertions.assertEquals(PRICE,
        wholesaleMappingRequest.getWholesaleConfig().get(0).getMinWholesaleDiscount().get(0)
            .getPrice());
  }

  @Test
  void toWholeMappingRequest_nullWholesaleConfigServiceRequest() {
    WholesaleMappingRequest wholesaleMappingRequest = new WholesaleMappingRequest();
    wholesaleMappingServiceRequest.setWholesaleConfig(Collections.singletonList(null));
    RequestHelper.toWholeMappingRequest(wholesaleMappingServiceRequest, wholesaleMappingRequest);
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesaleMappingRequest.getConfigurationType());
    Assertions.assertNull(wholesaleMappingRequest.getWholesaleConfig().get(0));
  }

  @Test
  void convertToAutoQcConfigRequest() throws  Exception {
    AutoQcConfigRequest autoQcConfigRequest =
        RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest);
    Assertions.assertTrue(autoQcConfigRequest.getRuleEnabled());
    Assertions.assertEquals(1, autoQcConfigRequest.getImageConfig().size());
  }

  @Test
  void convertToAutoQcConfigRequest_nullImageConfig() throws  Exception {
    autoQcConfigUpdateWebRequest.setImageConfig(Collections.singletonList(null));
    AutoQcConfigRequest autoQcConfigRequest =
        RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest);
    Assertions.assertTrue(autoQcConfigRequest.getRuleEnabled());
    Assertions.assertEquals(1, autoQcConfigRequest.getImageConfig().size());
  }

  @Test
  void validateAccessibilityForProductTab_WhenValidationDisabled() {
    // Test when validation is disabled
    RequestHelper.validateAccessibilityForProductTab(false, Collections.emptyList(), true,
      "ACCESS1,ACCESS2");
    // Should not throw any exception
  }

  @Test
  void validateAccessibilityForProductTab_WhenNotExternal() {
    // Test when not external
    RequestHelper.validateAccessibilityForProductTab(true, Collections.emptyList(), false,
      "ACCESS1,ACCESS2");
    // Should not throw any exception
  }

  @Test
  void validateAccessibilityForProductTab_WhenAccessibilityListEmpty() {
    // Test when accessibility list is empty
    RequestHelper.validateAccessibilityForProductTab(true, Collections.emptyList(), true, "");
    // Should not throw any exception
  }

  @Test
  void validateAccessibilityForProductTab_WhenAccessibilityListNull() {
    // Test when accessibility list is null
    RequestHelper.validateAccessibilityForProductTab(true, Collections.emptyList(), true, null);
    // Should not throw any exception
  }

  @Test
  void validateAccessibilityForProductTab_WhenUserHasRequiredAccess() {
    // Test when user has required access
    List<String> userAccessibilities = Arrays.asList("ACCESS1", "ACCESS2");
    RequestHelper.validateAccessibilityForProductTab(true, userAccessibilities, true,
      "ACCESS1,ACCESS2");
    // Should not throw any exception
  }

  @Test
  void validateAccessibilityForProductTab_WhenUserMissingRequiredAccess() {
    // Test when user is missing required access
    List<String> userAccessibilities = Arrays.asList("ACCESS1");
    try {
      RequestHelper.validateAccessibilityForProductTab(true, userAccessibilities, true,
        "ACCESS1,ACCESS2");
      Assertions.fail("Should have thrown UnauthorizedException");
    } catch (UnauthorizedException e) {
      Assertions.assertEquals(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE, e.getMessage());
    }
  }

  @Test
  void validateAccessibilityForProductTab_WhenUserHasNoAccess() {
    // Test when user has no access
    List<String> userAccessibilities = Collections.emptyList();
    try {
      RequestHelper.validateAccessibilityForProductTab(true, userAccessibilities, true,
        "ACCESS1,ACCESS2");
      Assertions.fail("Should have thrown UnauthorizedException");
    } catch (UnauthorizedException e) {
      Assertions.assertEquals(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE, e.getMessage());
    }
  }
}