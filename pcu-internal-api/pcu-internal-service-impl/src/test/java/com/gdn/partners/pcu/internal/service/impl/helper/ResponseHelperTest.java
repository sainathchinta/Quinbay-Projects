package com.gdn.partners.pcu.internal.service.impl.helper;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryCodeAndCategoryNameResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pcu.internal.client.model.request.Margin;
import com.gdn.partners.pcu.internal.client.model.response.AiGeneratedFieldsResponse;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHData;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCeritficationDetails;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductDetailCompleteResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.ImageQcConstants;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.web.model.enums.SellerStatus;
import com.gdn.partners.pcu.internal.web.model.enums.WorkflowWebState;
import com.gdn.partners.pcu.internal.web.model.response.AllowedAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthFilterResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCategoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionAndCategoryMappingWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImagePredictionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemAttributeValueWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSkuUpdateHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductUpdateHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductAttributeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductItemResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ItemNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.RestrictedKeywordsByFieldVendor;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.MasterCategoryResponse;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.google.common.collect.ImmutableMap;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ResponseHelperTest {

  private static final String ERROR_MESSAGE = "ERROR MESSAGE";
  private static final String ERROR_CODE = "ERROR_CODE";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String CODE = "CODE";
  private static final String PRODUCT_ID = "product_id";
  private static final String NAME = "NAME";
  private static final String NAME1 = "NAME1";
  private static final String CATEGORY_ID = "CATEGORY_ID";
  private static final String ID = "ID";
  private static final String pdpUrlPrefixLink = "prefix";
  private static final String NOTES_1 =
      "Diubah : [{field: 'Brand', oldValue: NI{KE}\nNIKE, newValue: Testing}, {field: 'Product Story', oldValue: null, newValue: change}]";
  private static final String NOTES_3 =
      "Diubah : [{field: 'Deskripsi', oldValue: Bhatara Batik Women BLZ/3057P/BNP Blazer Wanita, 3/4 sleeves blazer berbahan cotton yang didesain casual dalam motif batik dengan open front style. Bisa dipakai diberbagai acara formal atau non formal, karena dirancang dengan bahan yan1 nyaman dipakai dan tidak panas. Test2\n"
          + "\n" + "Size chart 1\n" + "\n" + "Size S/M\n" + "\n" + "Chest 94 cm, Length 59 cm\n" + "\n"
          + "Size L/XL\n" + "\n" + "1 test3\n" + "\n" + " \n" + "\n" + "Chest 100 cm, Length 62 cm\n" + "\n"
          + "wd dafsg \n" + "\n" + "dafsg fsgd\n" + "\n" + "#$#%$^%&\n" + "\n"
          + "& 4  sfd DFAGB, newValue: Bhatara Batik Women BLZ/3057P/BNP Blazer Wanita, 3/4 sleeves blazer berbahan cotton yang didesain casual dalam motif batik dengan open front style. Bisa dipakai diberbagai acara formal atau non formal, karena dirancang dengan bahan yan1 nyaman dipakai dan tidak panas. Test2\n"
          + "\n" + "Size chart 1\n" + "\n" + "Size S/M\n" + "\n" + "Chest 94 cm, Length 59 cm\n" + "\n"
          + "Size L/XL\n" + "\n" + "1 test3\n" + "\n" + " \n" + "\n" + "Chest 100 cm, Length 62 cm\n" + "\n"
          + "wd dafsg \n" + "\n" + "dafsg fsgd\n" + "\n" + "#$#%$^%&\n" + "\n"
          + "& 4  sfd DFAGBC}, {field: 'Unique Selling Point', oldValue: ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "$^%&^*&YZ\n" + "P\":LM>\n" + "weqrw\n" + "weqfwrg\n" + "efwgr\n" + "fewrge\n" + "fewgre\n"
          + "fewgr\n" + "e eqfgwr\n" + "ewrg\n" + "ewfrg\n" + "t e wfrg\n" + "ewrg\n" + "ergw\n"
          + "t t5y 4 rethr\n" + "hvjdavgjdavzcvfbsffs, newValue: ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "ramework.boot.test.mock.mockito.MockitoTestExecutionListener, org.springframework.boot.t\n"
          + "$^%&^*&YZ\n" + "P\":LM>\n" + "weqrw\n" + "weqfwrg\n" + "efwgr\n" + "fewrge\n" + "fewgre\n"
          + "fewgr\n" + "e eqfgwr\n" + "ewrg\n" + "ewfrg\n" + "t e wfrg\n" + "ewrg\n" + "ergw\n"
          + "• t t5y 4 rethr\n" + "hvjdavgjdavzcvfbsffs}]";
  private static final String NOTES = "Diubah : []";
  private static final String NOTES_2 =
      "Diubah : [{\"skuName\":null,\"field\":\"field1\",\"oldValue\":\"oldVal1\",\"newValue\":\"newVal1\"},"
          + "{\"skuName\":\"skuName2\",\"field\":\"field2\",\"oldValue\":\"oldVal2\",\"newValue\":\"newVal2\"},"
          + "{\"skuName\":null,\"field\":\"field3\",\"oldValue\":\"oldVal3\",\"newValue\":\"newVal3\"}]";
  private static final String NOTES_ASSERTION = "{field: 'Brand', oldValue: 'NIKE NIKE', newValue: 'Testing'}";
  private static final String SIMPLE_NOTE = "Something happened on this state";
  private static final String CATEGORY = "CATEGORY";
  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String SIZE_CHART_CODE = "SA-0000001";
  private static final String SIZE_CHART_NAME = "NAME";
  private static final String RECAT_REQUEST_CODE = "recat-request-code";
  private static final String PRODUCT_NAME = "productName";
  private static final String DEFAULT_CATEGORY_ID = "CGI-001";
  private static final String DEFAULT_NAME = "Name";
  private static final String DEFAULT_ATTRIBUTE_CODE = "ATC-001";
  private static final String DEFAULT_CODE = "CODE-001";
  private static final String DEFAULT_ATTRIBUTE_CODE_2 = "ATC-002";
  private static final String DEFAULT_ATTRIBUTE_TYPE = "DEFINING_ATTRIBUTE";
  private static final String DEFAULT_ATTRIBUTE_ID = "AID-001";
  private static final String DEFAULT_DESCRIPTION = "AID-001";
  private static final String FIELD_1 = "Brand";
  private static final String OLD_VALUE_1 = "NIKE NIKE";
  private static final String SKU_OLD_VALUE_1 = "oldVal1";
  private static final String SKU_OLD_VALUE_2 = "oldVal2";
  private static final String SKU_NEW_VALUE_1 = "newVal1";
  private static final String SKU_NEW_VALUE_2 = "newVal2";
  private static final String SKU_NAME = "skuName2";
  private static final String NEW_VALUE_1 = "Testing";
  private static final String FIELD_2 = "Product Story";
  private static final String FIELD_3 = "Deskripsi";
  private static final String FIELD_4 = "Unique Selling Point";
  private static final String SKU_FIELD_1 = "field1";
  private static final String DESCRIPTION = "Approve Image";
  private static final String ASSIGNEE_1 = "ASSIGNEE1";
  private static final String ASSIGNEE_2 = "ASSIGNEE2";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-0001";
  private static final String DEFAULT_ASSIGNEE_NAME = "blibli";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String ACTIVITY = "activity";
  private static final String UPDATED_BY = "updatedBy";
  public static final String CM = "CM";
  public static final String CC = "CC";
  public static final String SELLER_CODE = "sellerCode";
  public static final String CERTIFICATION_NUMBER = "certificationNumber";
  public static final Date ISSUED_DATE = new Date(2000, Calendar.MARCH,11);
  public static final Date EXPIRATION_DATE = new Date(2020, Calendar.MARCH,11);
  private VendorAssigneeResponse assigneeResponse;
  private List<VendorAssigneeResponse> assigneeResponseList = new ArrayList<>();
  private GdnRestListResponse<VendorAssigneeResponse> assigneeResponseGdnRestListResponse;
  private static final BaseResponse RESPONSE = new BaseResponse();
  private static final String URL = "url";
  private static final String LOCATION_PATH = "UlocationPath";
  private static final int SEQUENCE = 0;
  private static final boolean MAIN_IMAGE = true;
  private static final boolean RULE_ENABLED = true;
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String VALUE = "value";
  private static final String ATTRIBUTE_TYPE = "DEFINING_ATTRIBUTE";
  private static final String SKU_CODE = "skuCode";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String SKU_CODE_2 = "skuCode2";
  private static final String GENERATED_ITEM_NAME = "generatedItemName";
  private static final String UPC_CODE = "upcCode";
  private static final Integer DANGEROUS_GOODS_LEVEL = 0;
  private static final byte[] HASH = new byte[100];
  private static final String BUSINESS_PARTNER_CODE = "ABC-1100001";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String ACTIVE = "active";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_NAME = "category_name";
  private static final String REASON = "reason";
  private static final String STATUS = "reviewConfig";
  private static final String ITEM_NAME = "ITEM_NAME";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String ITEM_NAME_1 = "ITEM_NAME_1";
  private static final String ITEM_NAME_2 = "ITEM_NAME_2";
  private static final String ITEM_ATTRIBUTE_VALUE = "Value A";
  private static final String ITEM_ATTRIBUTE_VALUE_1 = "Value B";
  private static final String WARNA_ATTRIBUTE_NAME = "warna";
  private static final String NEUTRAL_STATUS = "Neutral";
  private static final String PREFIX = "prefix";
  private static final Long MERCHANT_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Long CATEGORY_CONFIGURATION_COUNT = Long.valueOf(20);
  private static final Long TOTAL_CONFIGURATION_COUNT = Long.valueOf(30);
  private static final String CREATED_BY = "createdBy";
  private static final String ACTIVITY_UPDATE = "Update";
  private static final String ACTIVITY_REGISTERED = "Registered";
  private static final String DAYS_ADDED = "daysAdded";
  private static final String STATUS_KEY = "status";
  private static final String TODAY = "today";
  private static final String YESTERDAY = "yesterday";
  private static final String TWO_DAYS_AGO = "twoDaysAgo";
  private static final String BETWEEN_3_UNTIL_5_DAYS_OLD = "threeToFiveDays";
  private static final String MORE_THAN_5_DAYS = "moreThanFiveDaysAgo";
  private static final String LOOKUP_GROUP = "DANGEROUS_GOODS_LEVEL";
  private static final String BRAND = "brand";
  private static final String SYSTEM_FEEDBACK = "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":true,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String SYSTEM_FEEDBACK_GOOD_IMAGE = "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":true,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":false,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String SYSTEM_FEEDBACK_GOOD_IMAGE_1 = "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":true,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"markForDelete\":true,\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":false,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String USER_FEEDBACK = "{\"userFeedback\":[{\"locationPath\":\"path1\",\"userPrediction\":[\"Text\",\"Blur\"]},{\"locationPath\":\"path2\",\"userPrediction\":[\"Text\"]}]}";
  private static final String USER_FEEDBACK_2 = "{\"userFeedback\":null}";
  private static final String WATERMARK_PRESENT = "watermarkpresent";
  private static final String SYSTEM_FEEDBCK_1 = "{\"timestamp\":1595003452436,\"productCode\":\"MTA-0451836\",\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451836/nike_image_qc_test_full01_sq4wy57o.jpeg\",\"hashCode\":\"e9778c56c1bf08c90d10238013ca8576\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}]}],\"success\":true,\"errorMessage\":\"\"}";
  private static final String USER_FEEDBACK_1 = "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451836/nike_image_qc_test_full01_sq4wy57o.jpeg\",\"userPrediction\":[\"Blur\"]}]}";
  private static final String NSFW_PRESENT = "nsfwpresent";
  private static final String TEXT = "Text";
  private static final String BLUR = "Blur";
  private static final String WATERMARK_PRESENT_IN = "watermarkpresentIn";
  private static final String NSFW_PRESENT_IN = "nsfwpresentIn";
  private static final String TEXT_IN = "TextIn";
  private static final String BLUR_IN = "BlurIn";
  private static final String PATH_1 = "path1";
  private static final String IMAGE_FILENAME = "/MTA-0451836/nike_image_qc_test_full01_sq4wy57o.jpeg";
  private static final String PATH_2 = "path2";
  private static final String GOOD = "Good";
  private static final String DESCRIPTIVE_ATTRIBUTE_TYPE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String DEFAULT_ATTRIBUTE_CODE_3 = "ATC-003";
  private static final String DEFAULT_ATTRIBUTE_ID_1 = "AID-002";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private static final String WHOLESALE_CONFIG_TYPE = "CONFIG_TYPE";
  private static final int QUANTITY = 3;
  private static final double PERCENTAGE = 10.0;
  private static final int QUANTITY_2 = 4;
  private static final double PERCENTAGE_2 = 12.0;
  private static final String PRODUCT_CENTER_ACTIVITY = "activity";
  private static final String PRODUCT_CENTER_DESCRIPTION = "description";
  private static final String PRODUCT_CENTER_UPDATEDBY = "updatedBy";
  private static final String PRODUCT_SKU = "productSku";
  private static final String NEW_CATEGORY_CODE = "newCategoryCode";
  private static final String NEW_CATEGORY_NAME = "newCategoryName";
  private static final String SYSTEM_FEEDBACK_WITH_EDITED_FLAG =
      "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0000001\",\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full01_uedfzhqd.jpeg\",\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}],\"edited\":false},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full02_gqm86hzf.jpeg\",\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":96},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}],\"edited\":false},{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":60},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":false,\"confidence\":80}],\"edited\":true},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":60},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":false,\"confidence\":80}],\"edited\":true}],\"success\":true,\"errorMessage\":\"\"}";
  private static final String SYSTEM_FEEDBACK_WITH_EDITED_FLAG_1 =
      "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0000001\",\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full01_uedfzhqd.jpeg\",\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}]},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full02_gqm86hzf.jpeg\",\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":96},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}]},{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":60},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":false,\"confidence\":80}],\"edited\":true},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":60},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":false,\"confidence\":80}],\"edited\":true}],\"success\":true,\"errorMessage\":\"\"}";
  private static final String RECAT_STATUS = "recatStatus";
  private static final String IMAGE_ASSIGNED = "Image Reviewer Assigned to system@vendor.com Assigned By system@vendor.com";
  private static final String IMAGE_ASSIGNED_NEW =
      "Ditugaskan : [{\"skuName\":null,\"field\":\"field1\"," + "\"oldValue\":\"oldVal1\",\"newValue\":\"newVal1\"}]";
  private static final String IMAGE_UNASSIGNED_NEW =
      "Not Assigned : [{\"skuName\":null,\"field\":\"field1\"," + "\"oldValue\":\"oldVal1\",\"newValue\":\"newVal1\"}]";
  private static final String CONTENT_ASSIGNED = "Content Reviewer Assigned to system@vendor.com Assigned By system@vendor.com";
  private static final String CONTENT_UN_ASSIGNED = "Content reviewer unassigned by system@mailinator.com";
  private static final String IMAGE_UN_ASSIGNED = "Image reviewer unassigned by system@mailinator.com";
  private static final String PREDICTION_TYPE = "predictionType";
  private static final String DISPLAY_NAME = "blur";
  private static final int CONFIDENCE_THRESHOLD = 50;
  private static final int TEXT_CONFIDENCE_THRESHOLD = 50;
  private static final int NEED_REVISION_CONFIENCE_THERSHOLD = 80;
  private static final String AUTO_NEED_REVISION = "Auto need revision";
  private static final String FORCE_REVIEW = "Force review";
  private static final String FIELD_IDENTIFIER = "field";
  private static final String RESTRICTED_KEYWORD = "keyword";
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "InternalProcessRequestCode";
  private static final String IMAGE_VIOLATION = "IMAGE_VIOLATION";
  private static final String PROHIBITED_DRUG = "PROHIBITED_DRUG";
  private static final String GOOGLE_RESTRICTED_KEYWORD = "GOOGLE_RESTRICTED_KEYWORD";
  private static final String IGNORE_IMAGE_QC = "Text,GOOGLE_RESTRICTED_KEYWORD,Brand mismatch";
  private static final String DONT_IGNORE_IMAGE_QC = "Blur,nsfwpresent,watermarkpresent,IMAGE_VIOLATION,No Detection";
  private static final String GCS_URL_PREFIX = "https://storage.googleapis.com/merchant-prod-image-static/source-image/";
  private static final String DEFAULT_LABEL_COLOUR = "BliLabelSuccess";
  private static final String DANGER_LABEL_COLOUR = "BliLabelDanger";
  private static final String WARNING_LABEL_COLOUR = "BliLabelWarning";
  private static final String ALLOWED_ATTRIBUTE_VALUE = "TEST";
  private static final String ALLOWED_ATTRIBUTE_VALUE_TYPE = "US";
  private static final boolean BRAND_FLAG = true;
  private static final boolean CATEGORY_FLAG = true;

  private ProductDetailResponse productDetailResponse;
  private List<ProductAttributeResponse> productAttributeResponses;
  private ProductAttributeResponse productAttributeResponse;
  private Set<ProductItemResponse> productItemResponses;
  private ProductItemResponse productItemResponse1;
  private ProductItemResponse productItemResponse2;
  private List<Image> images;
  private Image image;
  private List<ProductItemAttributeValueResponse> attributeValueResponses;
  private ProductItemAttributeValueResponse attributeValueResponse;
  private List<ProductCategoryResponse> productCategoryResponses;
  private ProductCategoryResponse productCategoryResponse;
  private CategoryResponse categoryResponse;
  private FilterCountResponse filterCountResponse;
  private List<ReviewProductResponse> reviewProductResponses;
  private ReviewProductResponse reviewProductResponse;
  private ProductHistoryResponse productHistoryResponse;
  private Map<String, Set<String>> pristineSupportedCategoryMap;
  private Set<String> categorySet;
  private List<ProductCodeResponse> productCodeResponses;
  private ProductCodeResponse productCodeResponse;
  private List<CategoryAttributeResponse> categoryAttributeResponses;
  private List<CategoryAttributeResponse> categoryAttributeResponses1;
  private List<ProductAttributeResponse> productAttributeResponses1;
  private CategoryAttributeResponse categoryAttributeResponse;
  private CategoryAttributeResponse attributeResponse1;
  private List<CategoryAttributeResponse> categoryAttributeResponses2;
  private List<CategoryAttributeResponse> categoryAttributeResponses3;
  private CategoryAttributeResponse attributeResponse2;
  private CategoryAttributeResponse attributeResponse3;
  private CategoryAttributeResponse attributeResponse4;

  private BrandRejectionInfoResponse brandRejectionInfoResponse;
  private BrandResponse brandResponse;
  private List<TaskHistoryResponse> taskHistoryResponseList;
  private DistributionProductDetailResponse distributionProductDetailResponse;
  private ProfileResponse profileResponse;
  private ProfileResponse profileResponse2;
  private List<ProfileResponse> profileResponseList;
  private List<ProfileResponse> profileResponseList2;
  private SuspensionProductResponse suspensionProductResponse;
  private List<SuspensionProductResponse> suspensionProductResponseList;
  private ProductSuspensionHistoryResponse productSuspensionHistoryResponse;
  private List<ProductSuspensionHistoryResponse> productSuspensionHistoryResponses;
  private List<MerchantNameResponse> merchantNameResponseList;
  private ProductCollectionCountRestResponse productCollectionCountRestResponse;
  private MapResponse mapResponse;
  private List<LookupResponse> lookupList = new ArrayList<>();
  private List<VendorCapacityDTO> vendorCapacityDTOList = new ArrayList<>();
  private ProductImageQcFeedbackResponse productImageQcFeedbackResponse;
  private List<PredictionTypeResponse> predictionTypeListResponse;
  private Map<String, ProfileResponse> profileResponseMap;

  private static final String BUSINESS_PARTNER_CODE_1 = "BUSINESS_PARTNER_CODE_1";
  private static final String BUSINESS_PARTNER_NAME_1 = "BUSINESS_PARTNER_NAME_1";
  private static final String BUSINESS_PARTNER_CODE_2 = "BUSINESS_PARTNER_CODE_2";
  private static final String BUSINESS_PARTNER_NAME_2 = "BUSINESS_PARTNER_NAME_2";
  private static final String INTERNAL = "INTERNAL";
  private static final String EXTERNAL = "EXTERNAL";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse =
      new ProductBusinessPartnerMapperResponse();
  private List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList = new ArrayList<>();
  private GdnRestListResponse<ProductBusinessPartnerMapperResponse>
      productBusinessPartnerMapperResponseGdnRestListResponse;
  private DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
  private List<DistributionProductResponse> distributionProductResponseList = new ArrayList<>();
  private IPRProductListResponse iprProductListResponse = new IPRProductListResponse();
  private List<IPRProductListResponse> iprProductListResponseList = new ArrayList<>();
  private VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
  private ProductDetailResponse productDetailResponseGroupByWarna;
  private GdnRestSingleResponse<ProductDetailResponse> responseGroupByWarna;
  private ProductItemResponse productItemResponse = new ProductItemResponse();
  private ProductItemResponse productItemResponse_1 = new ProductItemResponse();
  private ProductItemResponse productItemResponse_2 = new ProductItemResponse();
  private Set<ProductItemResponse> productItemWebResponseSet = new HashSet<>();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse1 =
      new ProductItemAttributeValueResponse();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse2 =
      new ProductItemAttributeValueResponse();
  private AttributeResponse attributeResponse = new AttributeResponse();
  private Set<ProductItemAttributeValueWebResponse> productItemAttributeValueWebResponseSet = new HashSet<>();
  private MerchantSearchResponse merchantSearchResponse;
  private ConfigurationStatusResponse configurationStatusResponse;
  private CategoryConfigurationFilterResponse categoryConfigurationFilterResponse =
      new CategoryConfigurationFilterResponse();
  private MerchantConfigurationFilterResponse merchantConfigurationFilterResponse =
      new MerchantConfigurationFilterResponse();
  private CategoryConfigurationHistoryResponse categoryConfigurationHistoryResponse =
      new CategoryConfigurationHistoryResponse();
  private MerchantConfigurationHistoryResponse merchantConfigurationHistoryResponse =
      new MerchantConfigurationHistoryResponse();
  private List<ProductCollectionResponse> productCollectionResponseList;
  private List<ProductHistoryResponse> productHistoryResponseList;
  private WholesaleMappingResponse wholesaleMappingResponse = new WholesaleMappingResponse();
  private WholesaleMappingResponse wholesaleMappingResponse2 = new WholesaleMappingResponse();
  private WholesaleConfigResponse wholesaleConfigResponse = new WholesaleConfigResponse();
  private ProductCenterHistoryResponse productCenterHistoryResponse;
  private ProductDetailCompleteResponse productDetailCompleteResponse = new ProductDetailCompleteResponse();
  private PreOrderResponse preOrderResponse;
  private RecatProductSummaryResponse recatProductSummaryResponse;
  private RecatProcessSummaryResponse recatProcessSummaryResponse;
  private List<ProductImagePredictionResponse> productImagePredictionResponseList;
  private ProductImagePredictionResponse productImagePredictionResponse;
  private RestrictedKeywordsByFieldVendor restrictedKeywordsByFieldVendor;

  @BeforeEach
  public void setUp() {
    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setId(PRODUCT_ID);
    productDetailResponse.setName(NAME);
    productDetailResponse.setPostLive(true);
    productDetailResponse.setReviewPending(true);
    productDetailResponse.setEdited(true);
    productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId(ATTRIBUTE_ID);
    attributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId(ID);
    attributeResponse.setName(NAME);
    attributeValueResponse.setAttributeResponse(attributeResponse);
    attributeValueResponse.setId(ID);
    attributeValueResponse.setValue(VALUE);
    attributeValueResponses = Arrays.asList(attributeValueResponse);
    productAttributeResponse.setAttribute(attributeResponse);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setId(ID);
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.MULTIPLE);
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setId(ID);
    allowedAttributeValueResponse.setAllowedAttributeCode(CODE);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(CODE);
    predefinedAllowedAttributeValueResponse.setId(ID);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    List<ProductAttributeValueResponse> valueResponses = Arrays.asList(productAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(valueResponses);
    productAttributeResponses = Arrays.asList(productAttributeResponse);
    productDetailResponse.setProductAttributeResponses(productAttributeResponses);
    productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(ID);
    productItemResponse1.setSkuCode(CODE);
    image = new Image();
    image.setId(ID);
    image.setEdited(true);
    image.setActive(true);
    images = Arrays.asList(image);
    productItemResponse1.setImages(images);
    productItemResponse1.setGeneratedItemName(NAME);
    productItemResponse1.setProductItemAttributeValueResponses(attributeValueResponses);
    productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setId(ID);
    productItemResponse2.setSkuCode(CODE);
    productItemResponse2.setImages(images);
    productItemResponse2.setGeneratedItemName(NAME1);
    productItemResponse2.setProductItemAttributeValueResponses(attributeValueResponses);
    productItemResponses = new HashSet<>();
    productItemResponses.add(productItemResponse2);
    productItemResponses.add(productItemResponse1);
    productDetailResponse.setProductItemResponses(productItemResponses);
    productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId(ID);
    categoryResponse = new CategoryResponse();
    categoryResponse.setName(NAME);
    categoryResponse.setCategoryCode(CODE);
    categoryResponse.setId(CATEGORY_ID);
    categoryResponse.setWholesalePriceConfigEnabled(true);
    productCategoryResponse.setCategory(categoryResponse);
    productCategoryResponses = Arrays.asList(productCategoryResponse);
    productDetailResponse.setProductCategoryResponses(productCategoryResponses);
    productDetailResponse.setCategories(Arrays.asList(CODE));
    productDetailResponse.setCategoriesEnglish(Arrays.asList(CODE));
    productDetailResponse.setImages(images);

    filterCountResponse = new FilterCountResponse();
    filterCountResponse.setAssigned(0);
    filterCountResponse.setUnassigned(100);
    filterCountResponse.setSourceDb(Boolean.FALSE);
    reviewProductResponses = new ArrayList<>();
    reviewProductResponse = new ReviewProductResponse();
    reviewProductResponse.setProductId(ID);
    reviewProductResponse.setProductName(NAME);
    reviewProductResponse.setProductCode(CODE);
    reviewProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    reviewProductResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    reviewProductResponse.setState(ACTIVE);
    reviewProductResponse.setCategoryCode(CATEGORY_CODE);
    reviewProductResponse.setCategoryName(CATEGORY_NAME);
    reviewProductResponses.add(reviewProductResponse);

    ReviewProductResponse reviewProductResponse1 = new ReviewProductResponse();
    reviewProductResponse1.setProductId(ID);
    reviewProductResponse1.setProductName(NAME);
    reviewProductResponse1.setProductCode(CODE);
    reviewProductResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    reviewProductResponse1.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    reviewProductResponse1.setState(ACTIVE);
    reviewProductResponse1.setCategoryCode(CATEGORY_CODE);
    reviewProductResponse1.setCategoryName(CATEGORY_NAME);
    reviewProductResponses.add(reviewProductResponse1);

    productHistoryResponse = new ProductHistoryResponse();
    productHistoryResponse.setProductId(PRODUCT_ID);
    productHistoryResponse.setNotes(NOTES);
    productHistoryResponse.setState(1);
    productHistoryResponse.setDescription(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE);

    pristineSupportedCategoryMap = new HashMap<>();
    categorySet = new HashSet<>();
    categorySet.add(CATEGORY_ID);
    pristineSupportedCategoryMap.put(CATEGORY, categorySet);

    productCodeResponses = new ArrayList<>();
    productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductCode(PRODUCT_CODE);
    productCodeResponse.setProductName(PRODUCT_NAME);
    productCodeResponses.add(productCodeResponse);

    categoryAttributeResponses = new ArrayList<>();
    categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(new AttributeResponse());
    categoryAttributeResponse.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    categoryAttributeResponse.getAttribute().setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    categoryAttributeResponse.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    categoryAttributeResponse.getAttribute().setMarkForDelete(false);
    categoryAttributeResponse.getAttribute().setName(DEFAULT_NAME);
    categoryAttributeResponse.setId(DEFAULT_CATEGORY_ID);
    categoryAttributeResponse.setMarkForDelete(false);
    categoryAttributeResponses.add(categoryAttributeResponse);

    categoryAttributeResponses1 = new ArrayList<>();
    attributeResponse1 = new CategoryAttributeResponse();
    attributeResponse1.setAttribute(new AttributeResponse());
    attributeResponse1.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse1.getAttribute().setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse1.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    attributeResponse1.getAttribute().setMarkForDelete(false);
    attributeResponse1.getAttribute().setName(DEFAULT_NAME);
    attributeResponse1.setId(DEFAULT_CATEGORY_ID);
    attributeResponse1.setMarkForDelete(false);
    categoryAttributeResponses1.add(attributeResponse1);

    productAttributeResponses1 = new ArrayList<>();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse1.getAttribute());
    productAttributeResponses1.add(productAttributeResponse);

    categoryAttributeResponses2 = new ArrayList<>();
    attributeResponse2 = new CategoryAttributeResponse();
    attributeResponse2.setAttribute(new AttributeResponse());
    attributeResponse2.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_3);
    attributeResponse2.getAttribute().setAttributeType(DESCRIPTIVE_ATTRIBUTE_TYPE);
    attributeResponse2.getAttribute().setId(DEFAULT_ATTRIBUTE_ID_1);
    attributeResponse2.getAttribute().setMarkForDelete(false);
    attributeResponse2.getAttribute().setName(DEFAULT_NAME);
    attributeResponse2.setId(DEFAULT_CATEGORY_ID);
    attributeResponse2.setMarkForDelete(false);
    attributeResponse2.getAttribute().setVariantCreatingUI(true);
    attributeResponse2.getAttribute().setVariantCreation(true);
    categoryAttributeResponses2.add(attributeResponse2);

    categoryAttributeResponses3 = new ArrayList<>();
    attributeResponse3 = new CategoryAttributeResponse();
    attributeResponse3.setAttribute(new AttributeResponse());
    attributeResponse3.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE_2);
    attributeResponse3.getAttribute().setAttributeType(DESCRIPTIVE_ATTRIBUTE_TYPE);
    attributeResponse3.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    attributeResponse3.getAttribute().setMarkForDelete(false);
    attributeResponse3.getAttribute().setName(DEFAULT_NAME);
    attributeResponse3.setId(DEFAULT_CATEGORY_ID);
    attributeResponse3.setMarkForDelete(false);
    attributeResponse3.getAttribute().setVariantCreatingUI(true);
    attributeResponse3.getAttribute().setVariantCreation(true);
    categoryAttributeResponses3.add(attributeResponse3);

    attributeResponse4 = new CategoryAttributeResponse();
    attributeResponse4.setAttribute(new AttributeResponse());
    attributeResponse4.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    attributeResponse4.getAttribute().setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    attributeResponse4.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    attributeResponse4.getAttribute().setMarkForDelete(false);
    attributeResponse4.getAttribute().setName(DEFAULT_NAME);
    attributeResponse4.setId(DEFAULT_CATEGORY_ID);
    attributeResponse4.setMarkForDelete(false);
    attributeResponse4.getAttribute().setVariantCreation(false);

    brandRejectionInfoResponse = new BrandRejectionInfoResponse();
    brandRejectionInfoResponse.setId(ID);
    brandRejectionInfoResponse.setRejectionReason(DEFAULT_DESCRIPTION);
    brandRejectionInfoResponse.setBrandName(DEFAULT_NAME);
    brandRejectionInfoResponse.setBrandRequestCode(DEFAULT_CODE);
    brandResponse = new BrandResponse();
    brandResponse.setId(ID);
    brandResponse.setBrandCode(DEFAULT_CODE);
    brandResponse.setBrandName(DEFAULT_NAME);

    taskHistoryResponseList = new ArrayList<>();
    TaskHistoryResponse taskHistoryResponse = new TaskHistoryResponse();
    taskHistoryResponse.setProductCode(PRODUCT_CODE);
    taskHistoryResponse.setReason(NOTES_ASSERTION);
    taskHistoryResponse.setState(DESCRIPTION);
    taskHistoryResponseList.add(taskHistoryResponse);

    distributionProductDetailResponse = new DistributionProductDetailResponse();
    distributionProductDetailResponse.setProductCode(PRODUCT_CODE);
    distributionProductDetailResponse.setProductName(PRODUCT_NAME);
    distributionProductDetailResponse.setVideoUrl(URL);
    distributionProductDetailResponse.setCategoryName(CATEGORY);
    distributionProductDetailResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    distributionProductDetailResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    distributionProductDetailResponse.setProductApproverAssignee(Constants.USER_NAME);
    distributionProductDetailResponse.setVendorCode(VENDOR_CODE);
    distributionProductDetailResponse.setProductApproved(true);
    distributionProductDetailResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    distributionProductDetailResponse.setEnableImageFeedback(true);
    distributionProductDetailResponse.setBrandCode(BRAND_CODE);
    distributionProductDetailResponse.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    distributionProductDetailResponse
        .setState(com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState.PASSED);
    distributionProductDetailResponse.setEdited(true);

    DistributionProductImageResponse distributionProductImageResponse = new DistributionProductImageResponse();
    distributionProductImageResponse.setLocationPath(LOCATION_PATH);
    distributionProductImageResponse.setSequence(SEQUENCE);
    distributionProductImageResponse.setMainImage(MAIN_IMAGE);
    distributionProductImageResponse.setEdited(true);
    distributionProductImageResponse.setActive(true);
    List<DistributionProductImageResponse> distributionProductImageResponseList = new ArrayList<>();
    distributionProductImageResponseList.add(distributionProductImageResponse);
    distributionProductDetailResponse.setProductImages(distributionProductImageResponseList);

    DistributionProductAttributeResponse distributionProductAttributeResponse = new DistributionProductAttributeResponse();
    distributionProductAttributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    distributionProductAttributeResponse.setName(NAME);
    distributionProductAttributeResponse.setAttributeType(VALUE);
    distributionProductAttributeResponse.setAttributeType(ATTRIBUTE_TYPE);
    distributionProductAttributeResponse.setVariantCreation(true);
    List<DistributionProductAttributeResponse> distributionProductAttributeResponseList = new ArrayList<>();
    distributionProductAttributeResponseList.add(distributionProductAttributeResponse);
    distributionProductDetailResponse.setProductAttributes(distributionProductAttributeResponseList);

    DistributionProductItemResponse distributionProductItemResponse = new DistributionProductItemResponse();
    distributionProductItemResponse.setSkuCode(SKU_CODE);
    distributionProductItemResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    distributionProductItemResponse.setUpcCode(UPC_CODE);
    distributionProductItemResponse.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);
    distributionProductItemResponse.setHash(HASH);
    distributionProductItemResponse.setProductItemImages(distributionProductImageResponseList);
    distributionProductItemResponse.setProductItemAttributes(distributionProductAttributeResponseList);
    List<DistributionProductItemResponse> distributionProductItemResponseList = new ArrayList<>();
    distributionProductItemResponseList.add(distributionProductItemResponse);
    DistributionProductItemResponse distributionProductItemResponse1 = new DistributionProductItemResponse();
    distributionProductItemResponse1.setSkuCode(SKU_CODE);
    distributionProductItemResponse1.setGeneratedItemName(GENERATED_ITEM_NAME);
    distributionProductItemResponse1.setUpcCode(UPC_CODE);
    distributionProductItemResponse1.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);
    distributionProductItemResponse1.setHash(HASH);
    distributionProductItemResponse1.setProductItemImages(distributionProductImageResponseList);
    distributionProductItemResponse1.setProductItemAttributes(distributionProductAttributeResponseList);
    distributionProductItemResponseList.add(distributionProductItemResponse1);
    distributionProductDetailResponse.setProductItems(distributionProductItemResponseList);

    productBusinessPartnerMapperResponse =
        new ProductBusinessPartnerMapperResponse(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_NAME_1);
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    productBusinessPartnerMapperResponse =
        new ProductBusinessPartnerMapperResponse(BUSINESS_PARTNER_CODE_2, BUSINESS_PARTNER_NAME_2);
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    productBusinessPartnerMapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(productBusinessPartnerMapperResponseList, new PageMetaData(), REQUEST_ID);

    assigneeResponse = new VendorAssigneeResponse(ASSIGNEE_1);
    assigneeResponseList.add(assigneeResponse);
    assigneeResponse = new VendorAssigneeResponse(ASSIGNEE_2);
    assigneeResponseList.add(assigneeResponse);
    assigneeResponseGdnRestListResponse =
        new GdnRestListResponse<>(assigneeResponseList, new PageMetaData(), REQUEST_ID);

    vendorDetailResponse.setAbleToReject(Boolean.TRUE);
    vendorDetailResponse.setVendorCode(VENDOR_CODE);

    distributionProductDetailResponse.setCurrentVendor(vendorDetailResponse);

    distributionProductResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    distributionProductResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    distributionProductResponse.setProductApproverAssignee(DEFAULT_ASSIGNEE_NAME);
    distributionProductResponse.setState(com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState.IN_REVIEW);
    distributionProductResponse.setCurrentVendor(vendorDetailResponse);
    distributionProductResponse.setProductApproved( Boolean.FALSE);
    distributionProductResponse.setDescription(DEFAULT_DESCRIPTION.getBytes());
    distributionProductResponse.setEdited(true);
    distributionProductResponseList.add(distributionProductResponse);

    iprProductListResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    iprProductListResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    iprProductListResponse.setAssignedTo(DEFAULT_ASSIGNEE_NAME);
    iprProductListResponseList.add(iprProductListResponse);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    companyDTO.setMerchantType(CC);
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);
    profileResponse.setCompany(companyDTO);
    profileResponseList = new ArrayList<>();
    profileResponseList.add(profileResponse);
    profileResponse2 = new ProfileResponse();
    profileResponse2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setInternationalFlag(true);
    companyDTO1.setMerchantType(CM);
    profileResponse2.setCompany(companyDTO1);
    profileResponseList2 = new ArrayList<>();
    profileResponseList2.add(profileResponse2);
    profileResponseMap = new HashMap<>();
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_2, profileResponse2);
    suspensionProductResponse = new SuspensionProductResponse();
    suspensionProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    suspensionProductResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    suspensionProductResponse.setCategoryCode(CATEGORY_CODE);
    suspensionProductResponse.setCategoryName(CATEGORY_NAME);
    suspensionProductResponse.setProductCode(PRODUCT_CODE);
    suspensionProductResponse.setProductName(PRODUCT_NAME);
    suspensionProductResponse.setProductSku(SKU_CODE);
    suspensionProductResponse.setState(ACTIVE);
    suspensionProductResponse.setItemSku(Arrays.asList(SKU_CODE_1, SKU_CODE_2));

    suspensionProductResponseList = new ArrayList<>();
    suspensionProductResponseList.add(suspensionProductResponse);

    productSuspensionHistoryResponse = new ProductSuspensionHistoryResponse();
    productSuspensionHistoryResponse.setProductSku(SKU_CODE);
    productSuspensionHistoryResponse.setReason(REASON);
    productSuspensionHistoryResponse.setStatus(STATUS);

    productSuspensionHistoryResponses = new ArrayList<>();
    productSuspensionHistoryResponses.add(productSuspensionHistoryResponse);

    merchantNameResponseList = new ArrayList<>();
    MerchantNameResponse merchantNameResponse = new MerchantNameResponse();
    merchantNameResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    merchantNameResponse.setMerchantName(BUSINESS_PARTNER_NAME);
    merchantNameResponseList.add(merchantNameResponse);


    productItemAttributeValueResponse.setValue(ITEM_ATTRIBUTE_VALUE);
    productItemAttributeValueResponse1.setValue(ITEM_ATTRIBUTE_VALUE_1);
    productItemAttributeValueResponse2.setValue(ITEM_ATTRIBUTE_VALUE);

    attributeResponse.setName(WARNA_ATTRIBUTE_NAME);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse1.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse2.setAttributeResponse(attributeResponse);

    productItemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));
    productItemResponse_1.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse1));
    productItemResponse_2.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse2));

    productItemResponse.setGeneratedItemName(ITEM_NAME);
    productItemResponse_1.setGeneratedItemName(ITEM_NAME_1);
    productItemResponse_2.setGeneratedItemName(ITEM_NAME_2);
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse_1.setSkuCode(SKU_CODE_1);
    productItemResponse_2.setSkuCode(SKU_CODE_2);

    productItemWebResponseSet.add(productItemResponse);
    productItemWebResponseSet.add(productItemResponse_1);
    productItemWebResponseSet.add(productItemResponse_2);

    productDetailResponseGroupByWarna = new ProductDetailResponse();
    productDetailResponseGroupByWarna.setId(PRODUCT_ID);
    productDetailResponseGroupByWarna.setProductCode(PRODUCT_CODE);
    productDetailResponseGroupByWarna.setProductItemResponses(productItemWebResponseSet);
    responseGroupByWarna = new GdnRestSingleResponse<>(productDetailResponseGroupByWarna, REQUEST_ID);

    merchantSearchResponse =
        new MerchantSearchResponse(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME, NEUTRAL_STATUS);
    configurationStatusResponse =
        ConfigurationStatusResponse.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .reviewConfig(NEUTRAL_STATUS).build();

    categoryConfigurationFilterResponse.setCreatedDate(new Date());
    categoryConfigurationFilterResponse.setCreatedBy(CREATED_BY);
    categoryConfigurationFilterResponse.setReviewConfig(Constants.PRE_LIVE_STATUS);
    categoryConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    categoryConfigurationFilterResponse.setCategoryCode(CATEGORY_CODE);

    merchantConfigurationFilterResponse.setCreatedBy(CREATED_BY);
    merchantConfigurationFilterResponse.setCreatedDate(new Date());
    merchantConfigurationFilterResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationFilterResponse.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    merchantConfigurationFilterResponse.setReviewConfig(Constants.POST_LIVE);

    categoryConfigurationHistoryResponse.setUpdatedDate(new Date());
    categoryConfigurationHistoryResponse.setCreatedDate(new Date());
    categoryConfigurationHistoryResponse.setCreatedBy(CREATED_BY);
    categoryConfigurationHistoryResponse.setUpdatedBy(UPDATED_BY);
    categoryConfigurationHistoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistoryResponse.setCategoryName(CATEGORY_NAME);
    categoryConfigurationHistoryResponse.setOldValue(Constants.PRE_LIVE_STATUS);
    categoryConfigurationHistoryResponse.setNewValue(Constants.POST_LIVE);
    categoryConfigurationHistoryResponse.setActivity(ACTIVITY);
    merchantConfigurationHistoryResponse.setOldValue(NEUTRAL_STATUS);
    merchantConfigurationHistoryResponse.setNewValue(Constants.POST_LIVE);
    merchantConfigurationHistoryResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistoryResponse.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistoryResponse.setActivity(ACTIVITY_REGISTERED);
    merchantConfigurationHistoryResponse.setCreatedBy(CREATED_BY);
    merchantConfigurationHistoryResponse.setCreatedDate(new Date());
    merchantConfigurationHistoryResponse.setUpdatedDate(new Date());
    merchantConfigurationHistoryResponse.setUpdatedBy(UPDATED_BY);

    productCollectionCountRestResponse = new ProductCollectionCountRestResponse();
    productCollectionCountRestResponse.setToday(1);
    productCollectionCountRestResponse.setYesterday(2);
    productCollectionCountRestResponse.setTwoDaysAgo(3);
    productCollectionCountRestResponse.setThreeUntilFiveDaysAgo(4);
    productCollectionCountRestResponse.setMoreThan5Days(5);

    mapResponse = new MapResponse();
    mapResponse.setMap(new HashMap<>());
    Map<String, Object> daysAdded = new HashMap<>();
    daysAdded.put(TODAY, 1L);
    Map<String, Object> status = new HashMap<>();
    daysAdded.put(ACTIVE, 2L);
    mapResponse.getMap().put(DAYS_ADDED, daysAdded);
    mapResponse.getMap().put(STATUS_KEY, status);

    LookupResponse lookup = new LookupResponse();
    lookup.setCode(CODE);
    lookup.setDescription(DESCRIPTION);
    lookup.setLookupGroup(LOOKUP_GROUP);
    lookup.setName(NAME);
    lookup.setOrderNumber(1);
    lookup.setId(ID);
    lookupList.add(lookup);

    productCollectionResponseList = new ArrayList<>();
    ProductCollectionResponse productCollectionResponse = new ProductCollectionResponse();
    productCollectionResponse.setImageApproved(true);
    productCollectionResponse.setContentApproved(true);
    productCollectionResponse.setProductName(PRODUCT_NAME);
    productCollectionResponse.setProductCode(PRODUCT_CODE);
    productCollectionResponse.setCategoryCode(CATEGORY_CODE);
    productCollectionResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollectionResponse.setBrand(BRAND);
    productCollectionResponse.setActivated(true);
    productCollectionResponse.setViewable(true);
    productCollectionResponseList.add(productCollectionResponse);

    VendorCapacityDTO vendorCapacityDTO = new VendorCapacityDTO();
    vendorCapacityDTO.setId(ID);
    vendorCapacityDTO.setVendorCode(VENDOR_CODE);
    vendorCapacityDTO.setName(NAME);
    vendorCapacityDTO.setRemainingCapacity(100000);
    vendorCapacityDTOList.add(vendorCapacityDTO);

    productImagePredictionResponse =new ProductImagePredictionResponse();
    productImagePredictionResponse.setPredictionType(PREDICTION_TYPE);
    productImagePredictionResponse.setDisplayName(DISPLAY_NAME);
    productImagePredictionResponse.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionResponse.setNeedRevisionConfidenceThreshold(NEED_REVISION_CONFIENCE_THERSHOLD);

    productImagePredictionResponseList=new ArrayList<>();
    productImagePredictionResponseList.add(productImagePredictionResponse);

    productImageQcFeedbackResponse = new ProductImageQcFeedbackResponse();
    productImageQcFeedbackResponse.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK);
    productImageQcFeedbackResponse.setUserFeedback(USER_FEEDBACK);

    predictionTypeListResponse = new ArrayList<>();
    predictionTypeListResponse.add(new PredictionTypeResponse(TEXT, TEXT_IN));
    predictionTypeListResponse.add(new PredictionTypeResponse(BLUR, BLUR_IN));
    predictionTypeListResponse.add(new PredictionTypeResponse(NSFW_PRESENT, NSFW_PRESENT_IN));
    predictionTypeListResponse.add(new PredictionTypeResponse(WATERMARK_PRESENT, WATERMARK_PRESENT_IN));
    predictionTypeListResponse.add(new PredictionTypeResponse(PROHIBITED_DRUG, PROHIBITED_DRUG));
    predictionTypeListResponse.add(new PredictionTypeResponse(GOOGLE_RESTRICTED_KEYWORD, GOOGLE_RESTRICTED_KEYWORD));

    productHistoryResponseList = new ArrayList<>();
    ProductHistoryResponse productHistoryWebResponse = new ProductHistoryResponse();
    productHistoryWebResponse.setDescription(DESCRIPTION);
    productHistoryWebResponse.setProductId(PRODUCT_ID);
    productHistoryWebResponse.setState(1);
    productHistoryWebResponse.setUpdatedBy(UPDATED_BY);
    productHistoryWebResponse.setCreatedBy(CREATED_BY);
    productHistoryWebResponse.setNotes(NOTES);
    productHistoryResponseList.add(productHistoryWebResponse);

    wholesaleMappingResponse.setWholesalePriceConfigEnabled(true);
    wholesaleMappingResponse.setConfigurationType(WHOLESALE_CONFIG_TYPE);
    wholesaleConfigResponse = WholesaleConfigResponse.builder().quantity(QUANTITY)
        .minWholesaleDiscount(Arrays.asList(new MinWholesaleDiscountResponse(null, PERCENTAGE))).build();
    wholesaleMappingResponse.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse));
    wholesaleMappingResponse2.setWholesalePriceConfigEnabled(true);
    wholesaleMappingResponse2.setConfigurationType(WHOLESALE_CONFIG_TYPE);
    wholesaleConfigResponse = WholesaleConfigResponse.builder().quantity(QUANTITY_2)
        .minWholesaleDiscount(Arrays.asList(new MinWholesaleDiscountResponse(null, PERCENTAGE_2))).build();
    wholesaleMappingResponse2.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse));

    productCenterHistoryResponse =
        ProductCenterHistoryResponse.builder().activity(PRODUCT_CENTER_ACTIVITY).description(PRODUCT_CENTER_DESCRIPTION)
            .updatedBy(PRODUCT_CENTER_UPDATEDBY).productSku(PRODUCT_SKU).updatedDate(new Date()).build();

    preOrderResponse =
        PreOrderResponse.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE)
            .preOrderDate(new Date()).build();

    recatProductSummaryResponse =
        new RecatProductSummaryResponse(RECAT_REQUEST_CODE, PRODUCT_CODE, PRODUCT_NAME, CATEGORY_CODE, CATEGORY_NAME,
            NEW_CATEGORY_CODE, NEW_CATEGORY_NAME, "PENDING", new Date(), "Succeed");

    recatProcessSummaryResponse = RecatProcessSummaryResponse.builder().initiator(CREATED_BY)
        .recatRequestCode(RECAT_REQUEST_CODE).productCount(10).status(RECAT_STATUS).build();

    restrictedKeywordsByFieldVendor =
      RestrictedKeywordsByFieldVendor.builder().keywords(Arrays.asList(RESTRICTED_KEYWORD)).fieldIdentifier(FIELD_IDENTIFIER)
        .build();
    distributionProductDetailResponse.setRestrictedKeywordsDetected(Arrays.asList(restrictedKeywordsByFieldVendor));
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestSingleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void testValidateErrorCodeResponse_ValidationError_ShouldThrowConstraintViolationException() {
    GdnBaseRestResponse clientResponse = new GdnBaseRestResponse();
    clientResponse.setSuccess(false);
    clientResponse.setErrorCode(ErrorCategory.VALIDATION.getCode());
    clientResponse.setErrorMessage("Validation error message");
    Exception exception = new Exception();
    try {
      ResponseHelper.validateErrorCodeResponse(clientResponse);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      assertEquals("Validation error message", exception.getMessage());
    }
  }

  @Test
  public void testValidateErrorCodeResponse_ValidationErrorCode() {
    GdnBaseRestResponse clientResponse = new GdnBaseRestResponse();
    clientResponse.setSuccess(false);
    clientResponse.setErrorCode(ErrorCategory.COMMUNICATION_FAILURE.getCode());
    clientResponse.setErrorMessage("Validation error message");
    Exception exception = new Exception();
    try {
      ResponseHelper.validateErrorCodeResponse(clientResponse);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      assertEquals(ApiIncorrectInputDataException.class, exception.getClass());
    }
  }

  @Test
  public void testValidateErrorCodeResponse_Null() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateErrorCodeResponse(null);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
    }
  }

  @Test
  public void testValidateErrorCodeResponse_UnspecifiedCode() {
    GdnBaseRestResponse clientResponse = new GdnBaseRestResponse();
    clientResponse.setSuccess(false);
    clientResponse.setErrorCode(ErrorCategory.UNSPECIFIED.getCode());
    clientResponse.setErrorMessage("Validation error message");
    Exception exception = new Exception();
    try {
      ResponseHelper.validateErrorCodeResponse(clientResponse);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
    }
  }

  @Test
  public void testValidateErrorCodeResponse_EmptyErrorCode() {
    GdnBaseRestResponse clientResponse = new GdnBaseRestResponse();
    clientResponse.setSuccess(false);
    clientResponse.setErrorCode(null);
    clientResponse.setErrorMessage("Validation error message");
    Exception exception = new Exception();
    try {
      ResponseHelper.validateErrorCodeResponse(clientResponse);
    } catch (Exception ex) {
      exception = ex;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_ValueNullTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSingleResponse_SuccessTrueTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, RESPONSE, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestSingleResponse));
  }

  @Test
  public void validateResponse_GdnRestListResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestListResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_ErrorMessageTest() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, "not working11", false, REQUEST_ID);
    try{
    ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);;
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, ex.getMessage());
    }
  }
  @Test
  public void validateResponse_GdnRestListResponse_SuccessFalseTest() {
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, false, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestListResponse_ValueNullTest() {
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, new PageMetaData(0, 10, 100), REQUEST_ID);
    ResponseHelper.validateResponse(gdnRestListResponse);
  }

  @Test
  public void validateResponse_GdnRestListResponse_SuccessTrueTest() {
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestListResponse));
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnBaseRestResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessFalseTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnBaseRestResponse_SuccessTrueTest() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnBaseRestResponse));
  }

  @Test
  public void validateResponse_ListBaseResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((ListBaseResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_ListBaseResponse_SuccessFalseTest() {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID, Collections.singletonList(RESPONSE),
            new Metadata(0, 10, (long) 100));
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(listBaseResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_ListBaseResponse_SuccessTrueTest() {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID, Collections.singletonList(RESPONSE),
            new Metadata(0, 10, (long) 100));
    assertTrue(ResponseHelper.validateResponse(listBaseResponse));
  }

  @Test
  public void validateResponse_GdnRestSimpleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestSimpleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSimpleResponse_SuccessFalseTest() {
    GdnRestSimpleResponse<BaseResponse> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID, RESPONSE);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSimpleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSimpleResponse_ValueNullTest() {
    GdnRestSimpleResponse<BaseResponse> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID, null);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSimpleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_GdnRestSimpleResponse_SuccessTrueTest() {
    GdnRestSimpleResponse<BaseResponse> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID, RESPONSE);
    assertTrue(ResponseHelper.validateResponse(gdnRestSimpleResponse));
  }

  @Test
  public void toProductDetailWebResponseTest_CategoryNullTest() {
    profileResponse.setMerchantStatus("INACTIVE");
    profileResponse.setSuspensionFlag(true);
    productDetailResponse.setCategories(null);
    ProductDetailWebResponse response = ResponseHelper.toProductDetailWebResponse(productDetailResponse, profileResponse);
    assertNull(response.getCategories());
    assertNull(response.getDocumentType());
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    assertEquals(ATTRIBUTE_ID, response.getProductAttributeResponses().get(0).getId());
    assertEquals(1, response.getImages().size());
    assertNotNull(response.getProductCategoryResponses());
    assertEquals(CODE, response.getProductCategoryResponses().get(0).getCategoryCode());
    assertEquals(2, response.getProductItemResponses().size());
    List<ProductItemWebResponse> productItemWebResponses =
        new ArrayList<ProductItemWebResponse>(response.getProductItemResponses());
    List<ProductCategoryWebResponse> productCategoryWebResponses =
        new ArrayList<>(response.getProductCategoryResponses());
    assertEquals(NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(NAME1, productItemWebResponses.get(1).getGeneratedItemName());
    assertTrue(response.isPostLive());
    assertTrue(response.isReviewPending());
    assertTrue(productCategoryWebResponses.get(0).isWholesalePriceConfigEnabled());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertEquals(SellerStatus.SUSPENDED.getStatus() , response.getSellerStatus());
  }

  @Test
  public void toProductDetailWebResponseTest() {
    profileResponse.setMerchantStatus("INACTIVE");
    profileResponse.setSuspensionFlag(false);
    ProductDetailWebResponse response = ResponseHelper.toProductDetailWebResponse(productDetailResponse, profileResponse);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    assertEquals(ATTRIBUTE_ID, response.getProductAttributeResponses().get(0).getId());
    assertEquals(1, response.getImages().size());
    assertEquals(1, response.getCategories().size());
    assertEquals(1, response.getCategoriesEnglish().size());
    assertNotNull(response.getProductCategoryResponses());
    assertEquals(CODE, response.getProductCategoryResponses().get(0).getCategoryCode());
    assertEquals(2, response.getProductItemResponses().size());
    List<ProductItemWebResponse> productItemWebResponses =
        new ArrayList<ProductItemWebResponse>(response.getProductItemResponses());
    List<ProductCategoryWebResponse> productCategoryWebResponses =
        new ArrayList<>(response.getProductCategoryResponses());
    assertEquals(NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(NAME1, productItemWebResponses.get(1).getGeneratedItemName());
    assertTrue(response.isPostLive());
    assertTrue(response.isReviewPending());
    assertTrue(productCategoryWebResponses.get(0).isWholesalePriceConfigEnabled());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertEquals(SellerStatus.INACTIVE.name(), response.getSellerStatus());
  }

  @Test
  public void toProductDetailWebResponseTest_warnaGroupingTest() {
    ProductDetailWebResponse response = ResponseHelper.toProductDetailWebResponse(productDetailResponseGroupByWarna, null);
    assertNotNull(response);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    List<ProductItemWebResponse> productItemWebResponses =
        new ArrayList<ProductItemWebResponse>(response.getProductItemResponses());
    assertEquals(ITEM_NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(ITEM_NAME_2, productItemWebResponses.get(1).getGeneratedItemName());
    assertEquals(ITEM_NAME_1, productItemWebResponses.get(2).getGeneratedItemName());
    assertFalse(response.isInternationalFlag());
    assertNull(response.getCommissionType());
  }

  @Test
  public void toProductDetailWebResponseTest_noGroupingTest() {
    productItemResponses.clear();
    productItemResponse.setProductItemAttributeValueResponses(new ArrayList<>());
    productItemResponse_1.setProductItemAttributeValueResponses(new ArrayList<>());
    productItemResponses.add(productItemResponse);
    productItemResponses.add(productItemResponse_1);
    productDetailResponseGroupByWarna.setProductItemResponses(productItemResponses);
    ProductDetailWebResponse response = ResponseHelper.toProductDetailWebResponse(productDetailResponseGroupByWarna, profileResponse);
    assertNotNull(response);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    List<ProductItemWebResponse> productItemWebResponses = new ArrayList<>(response.getProductItemResponses());
    assertEquals(ITEM_NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(ITEM_NAME_1, productItemWebResponses.get(1).getGeneratedItemName());
  }

  @Test
  public void toFilterCountWebResponseTest() {
    FilterCountWebResponse response = ResponseHelper.toFilterCountWebResponse(filterCountResponse);
    assertNotNull(response);
    assertEquals(0, response.getAssigned());
    assertEquals(100, response.getUnassigned());
    assertEquals(Boolean.FALSE, response.isSourceDb());
  }

  @Test
  public void toReviewProductWebResponseListTest() {
    List<ReviewProductWebResponse> responses =
        ResponseHelper.toReviewProductWebResponseList(reviewProductResponses, profileResponseMap);
    assertNotNull(responses);
    assertEquals(NAME, responses.get(0).getProductName());
    assertEquals(CC, responses.get(0).getCommissionType());
    assertEquals(CM, responses.get(1).getCommissionType());
    assertFalse(responses.get(0).isInternationalFlag());
    assertTrue(responses.get(1).isInternationalFlag());
  }

  @Test
  public void toProductHistoryWebResponseListTest() {
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(NOTES, response.get(0).getNotes().get(0));
  }

  @Test
  public void toBusinessPartnerWebResponseListFromProfileResponseTest() {
    List<BusinessPartnerWebResponse> response =
        ResponseHelper.toBusinessPartnerWebResponseListFromProfileResponse(profileResponseList);
    assertNotNull(response);
    assertEquals(BUSINESS_PARTNER_CODE_1, response.get(0).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_1, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void toBusinessPartnerWebResponseListFromProfileResponseWithoutBusinessPartnerNameTest() {
    List<BusinessPartnerWebResponse> response =
        ResponseHelper.toBusinessPartnerWebResponseListFromProfileResponse(profileResponseList2);
    assertNotNull(response);
    assertEquals(BUSINESS_PARTNER_CODE_2, response.get(0).getBusinessPartnerCode());
    assertEquals(StringUtils.EMPTY, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void toProductUpdateHistoryWebResponseListTest() throws JsonProcessingException {
    productHistoryResponse.setNotes(NOTES_1);
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    ObjectMapper objectMapper = new ObjectMapper();
    System.out.println(objectMapper.writeValueAsString(response.get(0).getProductUpdateHistoryWebResponseList()));
    System.out.println(response.get(0).getProductUpdateHistoryWebResponseList());
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(NOTES_ASSERTION, response.get(0).getNotes().get(0));
    assertEquals(FIELD_1, response.get(0).getProductUpdateHistoryWebResponseList().get(0).getField());
    assertEquals(OLD_VALUE_1, response.get(0).getProductUpdateHistoryWebResponseList().get(0).getOldValue());
    assertEquals(NEW_VALUE_1, response.get(0).getProductUpdateHistoryWebResponseList().get(0).getNewValue());
    assertEquals(FIELD_2, response.get(0).getProductUpdateHistoryWebResponseList().get(1).getField());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE, response.get(0).getDescription());
  }

  @Test
  public void toProductUpdateHistoryWebResponseListTest2() {
    productHistoryResponse.setNotes(NOTES_3);
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(FIELD_3, response.get(0).getProductUpdateHistoryWebResponseList().get(0).getField());
    assertEquals(FIELD_4, response.get(0).getProductUpdateHistoryWebResponseList().get(1).getField());
    assertNotNull(response.get(0).getProductUpdateHistoryWebResponseList().get(0).getOldValue());
    assertNotNull(response.get(0).getProductUpdateHistoryWebResponseList().get(0).getNewValue());
    assertNotNull(response.get(0).getProductUpdateHistoryWebResponseList().get(1).getOldValue());
    assertNotNull(response.get(0).getProductUpdateHistoryWebResponseList().get(1).getNewValue());
  }

  @Test
  public void toProductHistoryWebResponseListNotesEmptyTest() {
    productHistoryResponse.setNotes(StringUtils.EMPTY);
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertNull(response.get(0).getNotes());
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
  }

  @Test
  public void toProductHistoryWebResponseListNotesNullTest() {
    productHistoryResponse.setNotes(null);
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertNull(response.get(0).getNotes());
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
  }

  @Test
  public void toProductHistoryWebResponseListNotesTest() {
    productHistoryResponse.setNotes(NOTES_1);
    List<ProductHistoryWebResponse> response =
        ResponseHelper.toProductHistoryWebResponseList(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(NOTES_ASSERTION, response.get(0).getNotes().get(0));
  }

  @Test
  public void validateAndGetPristineSupportedCategoryTest() {
    String response = ResponseHelper.validateAndGetPristineSupportedCategory(CATEGORY_ID, pristineSupportedCategoryMap);
    assertEquals(response, CATEGORY);
  }

  @Test
  public void validateAndGetPristineSupportedCategoryEmptyTest() {
    String response = ResponseHelper.validateAndGetPristineSupportedCategory(CATEGORY_ID, Collections.EMPTY_MAP);
    assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void toProductSuggestionWebResponseListTest() {
    List<ProductSuggestionWebResponse> response =
        ResponseHelper.toProductSuggestionWebResponseList(productCodeResponses);
    assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, response.get(0).getProductName());
  }

  @Test
  public void categoryChangeCheck() {
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses1, categoryAttributeResponses1, productAttributeResponses1);
    Assertions.assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void categoryChangeCheckDifferentAttribute() {
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses, categoryAttributeResponses1, productAttributeResponses1);
    Assertions.assertEquals(ErrorMessages.DEFINING_ATTRIBUTE_MISMATCH, response);
  }

  @Test
  public void categoryChangeCheckListOfDefiningAttribute() {
    categoryAttributeResponses.add(attributeResponse1);
    categoryAttributeResponses1.add(categoryAttributeResponse);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(categoryAttributeResponse.getAttribute());
    productAttributeResponses1.add(productAttributeResponse);
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses, categoryAttributeResponses1, productAttributeResponses1);
    Assertions.assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void categoryChangeCheckForVariantTrue() {
    productAttributeResponses1.get(0).setAttribute(categoryAttributeResponses2.get(0).getAttribute());
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses2, categoryAttributeResponses2, productAttributeResponses1);
    Assertions.assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void categoryChangeCheckDifferentAttributeVariantTrue() {
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses2, categoryAttributeResponses3, productAttributeResponses1);
    Assertions.assertNotEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void categoryChangeCheckListOfAttributeVariantTrue() {
    categoryAttributeResponses2.add(attributeResponse3);
    categoryAttributeResponses3.add(attributeResponse2);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse2.getAttribute());
    productAttributeResponses1.add(productAttributeResponse);
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses2, categoryAttributeResponses3, productAttributeResponses1);
    Assertions.assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void categoryChangeCheckListOfDefiningAttributeAndVariantTrue() {
    categoryAttributeResponses2.add(attributeResponse1);
    categoryAttributeResponses3 = new ArrayList<>();
    categoryAttributeResponses3.add(attributeResponse1);
    categoryAttributeResponses3.add(attributeResponse2);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse2.getAttribute());
    productAttributeResponses1.add(productAttributeResponse);
    String response = ResponseHelper.categoryChangeCheck(categoryAttributeResponses2, categoryAttributeResponses3, productAttributeResponses1);
    Assertions.assertEquals(StringUtils.EMPTY, response);
  }

  @Test
  public void toBrandRejectionWebResponseTest() {
    BrandRejectionWebResponse brandRejectionWebResponse = ResponseHelper.toBrandRejectionWebResponse(brandRejectionInfoResponse);
    Assertions.assertNotNull(brandRejectionWebResponse);
    assertEquals(DEFAULT_DESCRIPTION, brandRejectionWebResponse.getRejectionReason());
    assertEquals(ID, brandRejectionWebResponse.getId());
    assertEquals(DEFAULT_NAME, brandRejectionWebResponse.getBrandName());
    assertEquals(DEFAULT_CODE, brandRejectionWebResponse.getBrandRequestCode());
  }

  @Test
  public void toBrandWebResponse() {
    BrandWebResponse response = ResponseHelper.toBrandWebResponse(brandResponse);
    assertNotNull(response);
    assertEquals(DEFAULT_CODE, response.getBrandCode());
    assertEquals(DEFAULT_NAME, response.getBrandName());
  }

  @Test
  public void fromTaskHistoryResponseToProductHistoryResponseTest() {
    List<ProductHistoryResponse> productHistoryResponseList =
        ResponseHelper.fromTaskHistoryResponseToProductHistoryResponse(taskHistoryResponseList);
    assertEquals(PRODUCT_CODE, productHistoryResponseList.get(0).getProductId());
    assertEquals(NOTES_ASSERTION, productHistoryResponseList.get(0).getNotes());
    assertEquals(DESCRIPTION, productHistoryResponseList.get(0).getDescription());
  }

  @Test
  public void convertDistributionDetailResponseToProductResponseTest() throws Exception {
    distributionProductDetailResponse.setPredictedBrand(BRAND);
    distributionProductDetailResponse.setImageViolations(Collections.singletonList(IMAGE_VIOLATION));
    distributionProductDetailResponse.getProductItems().get(0).setNewlyAdded(true);
    distributionProductDetailResponse.setShowProductUrl(true);
    distributionProductDetailResponse.getProductItems().get(0).setItemSku(ITEM_SKU);
    distributionProductDetailResponse.setDistributionMappingStatus(2);
    ProductDetailWebResponse response = ResponseHelper
        .convertDistributionDetailResponseToProductResponse(distributionProductDetailResponse, profileResponse, pdpUrlPrefixLink, pdpUrlPrefixLink);
    assertNotNull(response);
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(BUSINESS_PARTNER_CODE_1, response.getBusinessPartnerCode());
    assertTrue(response.isEnableImageFeedback());
    assertEquals(BRAND_CODE, response.getBrandCode());
    assertEquals(BRAND_APPROVAL_STATUS, response.getBrandApprovalStatus());
    assertEquals(2, response.getDistributionMappingStatus());
    List<ProductItemWebResponse> productItemWebResponses = new ArrayList<>(response.getProductItemResponses());
    assertTrue(productItemWebResponses.get(0).getProductItemAttributeValueResponses().get(0).getAttribute()
        .isVariantCreation());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertTrue(response.isEdited());
    assertTrue(response.getImages().get(0).isEdited());
    assertTrue(productItemWebResponses.get(0).getImages().get(0).isEdited());
    assertEquals(FIELD_IDENTIFIER, response.getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
    assertEquals(RESTRICTED_KEYWORD, response.getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
    assertEquals(BRAND, response.getPredictedBrand());
    assertEquals(response.getImageViolations(), Collections.singletonList(IMAGE_VIOLATION));
    assertTrue(productItemWebResponses.stream().anyMatch(ProductItemWebResponse::isNewlyAdded));
    assertTrue(response.isShowProductUrl());
  }

  @Test
  public void convertDistributionDetailResponseToProductResponseNeedRevisionNotesTest() throws Exception {
    distributionProductDetailResponse.setProductNotes(
        ProductNotesResponse.builder().vendorNotes(Arrays.asList("notes")).contentAdditionalNotes("note")
            .imagesAdditionalNotes("notes").allVariants(true).build());
    distributionProductDetailResponse.getProductItems().get(0).setItemNotes(
        ItemNotesResponse.builder().vendorNotes(Arrays.asList("notes")).vendorErrorFields(Arrays.asList("url"))
            .build());
    ProductDetailWebResponse response =
        ResponseHelper.convertDistributionDetailResponseToProductResponse(distributionProductDetailResponse, profileResponse, pdpUrlPrefixLink, pdpUrlPrefixLink);
    assertNotNull(response);
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(BUSINESS_PARTNER_CODE_1, response.getBusinessPartnerCode());
    assertTrue(response.isEnableImageFeedback());
    assertEquals(BRAND_CODE, response.getBrandCode());
    assertEquals(BRAND_APPROVAL_STATUS, response.getBrandApprovalStatus());
    List<ProductItemWebResponse> productItemWebResponses = new ArrayList<>(response.getProductItemResponses());
    assertTrue(productItemWebResponses.get(0).getProductItemAttributeValueResponses().get(0).getAttribute()
        .isVariantCreation());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertTrue(response.isEdited());
    assertTrue(response.getImages().get(0).isEdited());
    assertTrue(productItemWebResponses.get(0).getImages().get(0).isEdited());
    assertEquals("note", response.getNeedRevisionNotes().getContentAdditionalNotes());
    assertEquals(1, response.getNeedRevisionNotes().getItemNotes().get(0).getVendorErrorFields().size());
    assertEquals(FIELD_IDENTIFIER, response.getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
    assertEquals(RESTRICTED_KEYWORD, response.getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
  }

  @Test
  public void convertDistributionDetailResponseToVendorProductResponseTest() throws Exception {
    ProductDetailWebResponse response = ResponseHelper
        .convertDistributionDetailResponseToProductDetailWebResponse(distributionProductDetailResponse, null, pdpUrlPrefixLink, pdpUrlPrefixLink);
    assertNotNull(response);
    assertEquals(PRODUCT_CODE, response.getProductCode());
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals(BUSINESS_PARTNER_CODE_1, response.getBusinessPartnerCode());
    assertTrue(response.getCurrentVendor().getIsAbleToReject());
    assertEquals(BUSINESS_PARTNER_NAME, response.getBusinessPartnerName());
    assertEquals(CATEGORY, response.getCategories().get(0));
    assertEquals(URL, response.getUrl());
    assertEquals(VENDOR_CODE, response.getCurrentVendor().getVendorCode());
    Assertions.assertFalse(response.isContentApproved());
    Assertions.assertFalse(response.isImageApproved());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, new String(response.getDescription()));
    assertEquals(VENDOR_CODE, response.getVendorCode());
    assertEquals(WorkflowWebState.PASSED.name(), response.getState());
    assertFalse(response.isInternationalFlag());
    assertNull(response.getCommissionType());
  }

  @Test
  public void convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponseTest() {
    List<ProductBusinessPartnerMapperWebResponse> response = ResponseHelper
        .convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponse(
            productBusinessPartnerMapperResponseGdnRestListResponse);
    assertEquals(BUSINESS_PARTNER_CODE_1, response.get(0).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_1, response.get(0).getBusinessPartnerName());
    assertEquals(BUSINESS_PARTNER_CODE_2, response.get(1).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_2, response.get(1).getBusinessPartnerName());
  }

  @Test
  public void convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponseTestInternal() {
    List<ProductBusinessPartnerMapperResponse> productBusinessPartnerMapperResponseList1 = new ArrayList<>();

    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperWebResponse1 =
        new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperWebResponse1.setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);
    productBusinessPartnerMapperWebResponse1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    productBusinessPartnerMapperResponseList1.add(productBusinessPartnerMapperWebResponse1);

    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse2 =
        new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse2.setBusinessPartnerCode(INTERNAL);
    productBusinessPartnerMapperResponse2.setBusinessPartnerName(INTERNAL);
    productBusinessPartnerMapperResponseList1.add(productBusinessPartnerMapperResponse2);

    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse3 =
        new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse3.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_2);
    productBusinessPartnerMapperResponse3.setBusinessPartnerName(BUSINESS_PARTNER_NAME_2);
    productBusinessPartnerMapperResponseList1.add(productBusinessPartnerMapperResponse3);

    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse4 =
        new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse4.setBusinessPartnerCode(EXTERNAL);
    productBusinessPartnerMapperResponse4.setBusinessPartnerName(EXTERNAL);
    productBusinessPartnerMapperResponseList1.add(productBusinessPartnerMapperResponse4);

    GdnRestListResponse<ProductBusinessPartnerMapperResponse> listResponse =
        new GdnRestListResponse(productBusinessPartnerMapperResponseList1, new PageMetaData(1, 4, 4L), REQUEST_ID);

    List<ProductBusinessPartnerMapperWebResponse> response =
        ResponseHelper.convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponse(listResponse);
    assertEquals(BUSINESS_PARTNER_CODE_1, response.get(0).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_1, response.get(0).getBusinessPartnerName());
    assertEquals(BUSINESS_PARTNER_CODE_2, response.get(1).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_2, response.get(1).getBusinessPartnerName());
  }

  @Test
  public void convertAssigneeResponseToAssigneeWebResponseTest() {
    List<AssigneeWebResponse> response =
        ResponseHelper.convertAssigneeResponseToAssigneeWebResponse(assigneeResponseGdnRestListResponse);
    assertEquals(ASSIGNEE_1, response.get(0).getAssigneeEmailId());
    assertEquals(ASSIGNEE_2, response.get(1).getAssigneeEmailId());
  }

  @Test
  public void fromDistributionProductResponseToDistributionProductWebResponseTest() {
    distributionProductResponseList.get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    distributionProductResponseList.get(0)
      .setImageViolations(Arrays.asList(PRODUCT_CODE, PRODUCT_SKU));
    distributionProductResponseList.get(0)
        .setPredictedBrand(BRAND);
    profileResponse.setOfficial(true);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1,profileResponse);
    List<DistributionProductWebResponse> response =
        ResponseHelper.fromDistributionProductResponseToDistributionProductWebResponse(distributionProductResponseList, profileResponseMap);
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, response.get(0).getProductApproverAssignee());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_CODE_1,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(VENDOR_CODE, response.get(0).getCurrentVendor().getVendorCode());
    Assertions.assertFalse(response.get(0).isProductApproved());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, new String(response.get(0).getDescription()));
    assertEquals(CC, response.get(0).getCommissionType());
    assertFalse(response.get(0).isInternationalFlag());
    assertTrue(response.get(0).isEdited());
    assertEquals(PRODUCT_CODE, response.get(0).getImageViolations().get(0));
    assertEquals(PRODUCT_SKU, response.get(0).getImageViolations().get(1));
    Assertions.assertEquals(BRAND, response.get(0).getPredictedBrand());
    Assertions.assertEquals(true,response.get(0).isOfficialSeller());
  }

  @Test
  public void fromDistributionProductResponseToDistributionProductWebResponseTest_nullState() {
    distributionProductResponseList.get(0).setState(null);
    List<DistributionProductWebResponse> response =
        ResponseHelper.fromDistributionProductResponseToDistributionProductWebResponse(distributionProductResponseList, profileResponseMap);
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, response.get(0).getProductApproverAssignee());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertEquals(VENDOR_CODE, response.get(0).getCurrentVendor().getVendorCode());
    Assertions.assertFalse(response.get(0).isProductApproved());
    Assertions.assertNull(response.get(0).getState());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, new String(response.get(0).getDescription()));
    assertNull(response.get(0).getCommissionType());
  }

  @Test
  public void fromDistributionProductResponseToDistributionProductWebResponseTest_nullCurrentVendor() {
    distributionProductResponseList.get(0).setCurrentVendor(null);
    List<DistributionProductWebResponse> response =
        ResponseHelper.fromDistributionProductResponseToDistributionProductWebResponse(distributionProductResponseList, profileResponseMap);
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME, response.get(0).getProductApproverAssignee());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_BUSINESS_PARTNER_NAME,
        response.get(0).getBusinessPartnerName());
    Assertions.assertFalse(response.get(0).isProductApproved());
    Assertions.assertEquals(WorkflowWebState.IN_REVIEW, response.get(0).getState());
    Assertions.assertNull(response.get(0).getCurrentVendor());
    Assertions.assertEquals(DEFAULT_DESCRIPTION, new String(response.get(0).getDescription()));
    assertNull(response.get(0).getCommissionType());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseTest() {
    productHistoryResponse.setNotes(NOTES_2);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    ProductUpdateHistoryWebResponse productUpdateHistoryWebResponse =
        response.get(0).getProductUpdateHistoryWebResponseList().get(0);
    assertEquals(SKU_OLD_VALUE_1, productUpdateHistoryWebResponse.getOldValue());
    assertEquals(SKU_NEW_VALUE_1, productUpdateHistoryWebResponse.getNewValue());
    assertEquals(SKU_FIELD_1, productUpdateHistoryWebResponse.getField());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE, response.get(0).getDescription());
    assertEquals(SKU_NAME,
        ((ProductSkuUpdateHistoryWebResponse) response.get(0).getProductUpdateHistoryWebResponseList().get(1))
            .getSkuName());
  }

  @Test
  public void mapToIPRProductWebResponseNoMatchingBusinessPartnerCode() {
    iprProductListResponseList.get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setOfficial(true);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    ResponseHelper.mapToIPRProductWebResponse(iprProductListResponseList, profileResponseMap,
        PREFIX);
    assertNull(iprProductListResponseList.get(0).getCommissionType());
  }

  @Test
  public void testMapToIPRProductWebResponseNullCompanyInProfileResponse() {
    iprProductListResponseList.get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    profileResponse.setOfficial(true);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    profileResponse.setCompany(null);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    ResponseHelper.mapToIPRProductWebResponse(iprProductListResponseList, profileResponseMap,
        PREFIX);
    assertNull(iprProductListResponseList.get(0).getCommissionType());
  }

  @Test
  public void testMapToIPRProductWebResponseMatchingBusinessPartnerCode() {
    iprProductListResponseList.get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    profileResponse.setOfficial(true);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    CompanyDTO company = new CompanyDTO();
    company.setInternationalFlag(true);
    company.setMerchantType(CC);
    profileResponse.setCompany(company);
    profileResponse.setOfficial(true);
    profileResponseMap.put(BUSINESS_PARTNER_CODE_1, profileResponse);
    ResponseHelper.mapToIPRProductWebResponse(iprProductListResponseList, profileResponseMap,
        PREFIX);
    assertTrue(iprProductListResponseList.get(0).isInternationalFlag());
    assertEquals(CC, iprProductListResponseList.get(0).getCommissionType());
    assertTrue(iprProductListResponseList.get(0).isOfficialSeller());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseNullTest() {
    productHistoryResponse.setNotes(null);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UPDATE, response.get(0).getDescription());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseAssignedTest() {
    productHistoryResponse.setNotes(IMAGE_ASSIGNED);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT, response.get(0).getDescription());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseAssignedNewTest() {
    productHistoryResponse.setNotes(IMAGE_ASSIGNED_NEW);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT, response.get(0).getDescription());
    assertEquals(1, response.get(0).getProductUpdateHistoryWebResponseList().size());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseUnAssignedNewTest() {
    productHistoryResponse.setNotes(IMAGE_UNASSIGNED_NEW);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT, response.get(0).getDescription());
    assertEquals(1, response.get(0).getProductUpdateHistoryWebResponseList().size());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseContentAssignedTest() {
    productHistoryResponse.setNotes(CONTENT_ASSIGNED);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_ASSIGNMENT, response.get(0).getDescription());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseUnAssignedTest() {
    productHistoryResponse.setNotes(IMAGE_UN_ASSIGNED);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT, response.get(0).getDescription());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseContentUnAssignedTest() {
    productHistoryResponse.setNotes(CONTENT_UN_ASSIGNED);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(Constants.PRODUCT_HISTORY_DESCRIPTION_FOR_UN_ASSIGNMENT, response.get(0).getDescription());
  }

  @Test
  public void toProductHistoryWebResponseListFromVendorHistoryResponseSimpleReasonTest() {
    productHistoryResponse.setNotes(SIMPLE_NOTE);
    productHistoryResponse.setDescription(ACTIVITY);
    List<ProductHistoryWebResponse> response = ResponseHelper
        .toProductHistoryWebResponseListFromVendorHistoryResponse(Collections.singletonList(productHistoryResponse));
    assertNotNull(response);
    assertEquals(PRODUCT_ID, response.get(0).getProductId());
    assertEquals(ACTIVITY, response.get(0).getDescription());
    assertEquals(SIMPLE_NOTE, response.get(0).getNotes().get(0));
    assertNull(response.get(0).getProductUpdateHistoryWebResponseList());
  }

  @Test
  public void toProductSuspensionWebResponseListTest() {
    suspensionProductResponseList.get(0).setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    List<ProductSuspensionWebResponse> responses =
        ResponseHelper.toProductSuspensionWebResponseList(suspensionProductResponseList, profileResponseMap);
    assertNotNull(responses);
    assertEquals(PRODUCT_NAME, responses.get(0).getProductName());
    assertEquals(PRODUCT_CODE, responses.get(0).getProductCode());
    assertEquals(SKU_CODE, responses.get(0).getProductSku());
    assertEquals(BUSINESS_PARTNER_CODE_1, responses.get(0).getBusinessPartnerCode());
    assertEquals(CC, responses.get(0).getCommissionType());
    assertFalse(responses.get(0).isInternationalFlag());
    assertEquals(BUSINESS_PARTNER_NAME, responses.get(0).getBusinessPartnerName());
    assertEquals(CATEGORY_CODE, responses.get(0).getCategoryCode());
    assertEquals(CATEGORY_NAME, responses.get(0).getCategoryName());
    assertEquals(2, responses.get(0).getItemSku().size());
    assertEquals(SKU_CODE_1, responses.get(0).getItemSku().get(0));
    assertEquals(SKU_CODE_2, responses.get(0).getItemSku().get(1));
    assertEquals(ACTIVE, responses.get(0).getStatus());
  }

  @Test
  public void toBusinessPartnerNameMap() {
    Map<String, String> responses = ResponseHelper.toBusinessPartnerNameMap(merchantNameResponseList);
    assertNotNull(responses);
    assertEquals(1, responses.size());
    assertEquals(BUSINESS_PARTNER_NAME, responses.get(BUSINESS_PARTNER_CODE));
  }

  @Test
  public void toProductSuspensionHistoryWebResponseListTest() {
    List<ProductSuspensionHistoryWebResponse> responses =
        ResponseHelper.toProductSuspensionHistoryWebResponseList(productSuspensionHistoryResponses);
    assertNotNull(responses);
    assertEquals(SKU_CODE, responses.get(0).getProductSku());
    assertEquals(REASON, responses.get(0).getReason());
    assertEquals(STATUS, responses.get(0).getStatus());

  }

  @Test
  public void toMerchantWebSearchResponseTest() {
    List<MerchantWebSearchResponse> merchantWebSearchResponseList =
        ResponseHelper.toMerchantWebSearchResponse(Arrays.asList(merchantSearchResponse));
    assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantWebSearchResponseList.get(0).getMerchantCode());
    assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantWebSearchResponseList.get(0).getMerchantName());
    assertEquals(NEUTRAL_STATUS, merchantWebSearchResponseList.get(0).getReviewConfig());
  }

  @Test
  public void toConfigurationsStatusWebResponseTest() {
    List<ConfigurationsStatusWebResponse> configurationsStatusWebResponseList =
        ResponseHelper.toConfigurationsStatusWebResponse(Arrays.asList(configurationStatusResponse));
    assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, configurationsStatusWebResponseList.get(0).getMerchantCode());
    assertEquals(CATEGORY_CODE, configurationsStatusWebResponseList.get(0).getCategoryCode());
    assertEquals(NEUTRAL_STATUS, configurationsStatusWebResponseList.get(0).getReviewConfig());
  }

  @Test
  public void toConfigurationCountWebResponseTest() {
    ConfigurationCountWebResponse configurationCountWebResponse = ResponseHelper.toConfigurationCountWebResponse(
        new ConfigurationCountResponse(CATEGORY_CONFIGURATION_COUNT, MERCHANT_CONFIGURATION_COUNT));
    assertEquals(CATEGORY_CONFIGURATION_COUNT, configurationCountWebResponse.getCategoryConfigurationCount());
    assertEquals(MERCHANT_CONFIGURATION_COUNT, configurationCountWebResponse.getMerchantConfigurationCount());
    assertEquals(TOTAL_CONFIGURATION_COUNT, configurationCountWebResponse.getTotalConfigurationCount());
  }

  @Test
  public void toCategoryConfigurationFilterWebResponseListTest() {
    List<CategoryConfigurationFilterWebResponse> categoryConfigurationFilterWebResponseList =
        ResponseHelper.toCategoryConfigurationFilterWebResponseList(Arrays.asList(categoryConfigurationFilterResponse));
    Assertions.assertEquals(CREATED_BY, categoryConfigurationFilterWebResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationFilterWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationFilterWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationFilterWebResponseList.get(0).getReviewConfig());
    Assertions.assertNotNull(categoryConfigurationFilterWebResponseList.get(0).getCreatedDate());
  }

  @Test
  public void toMerchantConfigurationFilterWebResponseListTest() {
    List<MerchantConfigurationFilterWebResponse> merchantConfigurationFilterWebResponseList =
        ResponseHelper.toMerchantConfigurationFilterWebResponseList(Arrays.asList(merchantConfigurationFilterResponse));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        merchantConfigurationFilterWebResponseList.get(0).getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        merchantConfigurationFilterWebResponseList.get(0).getMerchantCode());
    Assertions.assertEquals(CATEGORY_NAME, merchantConfigurationFilterWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(Constants.POST_LIVE, merchantConfigurationFilterWebResponseList.get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationFilterWebResponseList.get(0).getCreatedBy());
    Assertions.assertNotNull(merchantConfigurationFilterWebResponseList.get(0).getCreatedDate());

  }

  @Test
  public void toCategoryConfigurationHistoryWebResponseListTest() {
    List<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponseList = ResponseHelper
        .toCategoryConfigurationHistoryWebResponseList(Arrays.asList(categoryConfigurationHistoryResponse));
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getCreatedDate());
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getUpdatedDate());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationHistoryWebResponseList.get(0).getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, categoryConfigurationHistoryWebResponseList.get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY, categoryConfigurationHistoryWebResponseList.get(0).getActivity());
  }

  @Test
  public void toCategoryConfigurationHistoryWebResponseListTest_activityUpdate() {
    categoryConfigurationHistoryResponse.setActivity(ACTIVITY_UPDATE);
    List<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponseList = ResponseHelper
        .toCategoryConfigurationHistoryWebResponseList(Arrays.asList(categoryConfigurationHistoryResponse));
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getCreatedDate());
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getUpdatedDate());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationHistoryWebResponseList.get(0).getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, categoryConfigurationHistoryWebResponseList.get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY_UPDATE, categoryConfigurationHistoryWebResponseList.get(0).getActivity());
    Assertions.assertEquals(Constants.ACTIVITY_UPDATE_IN,
        categoryConfigurationHistoryWebResponseList.get(0).getActivityIn());
  }

  @Test
  public void toCategoryConfigurationHistoryWebResponseListTest_activityRegistered() {
    categoryConfigurationHistoryResponse.setActivity(ACTIVITY_REGISTERED);
    List<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponseList = ResponseHelper
        .toCategoryConfigurationHistoryWebResponseList(Arrays.asList(categoryConfigurationHistoryResponse));
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getCreatedDate());
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponseList.get(0).getUpdatedDate());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryWebResponseList.get(0).getCategoryName());
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryWebResponseList.get(0).getCreatedBy());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationHistoryWebResponseList.get(0).getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, categoryConfigurationHistoryWebResponseList.get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY_REGISTERED, categoryConfigurationHistoryWebResponseList.get(0).getActivity());
    Assertions.assertEquals(Constants.ACTIVITY_REGISTERED_IN,
        categoryConfigurationHistoryWebResponseList.get(0).getActivityIn());
  }

  @Test
  public void toMerchantConfigurationHistoryWebResponseTest() {
    MerchantConfigurationHistoryWebResponse merchantConfigurationHistoryWebResponse =
        ResponseHelper.toMerchantConfigurationHistoryWebResponse(merchantConfigurationHistoryResponse);
    Assertions.assertNotNull(merchantConfigurationHistoryWebResponse.getCreatedDate());
    Assertions.assertNotNull(merchantConfigurationHistoryWebResponse.getUpdatedDate());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantConfigurationHistoryWebResponse.getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantConfigurationHistoryWebResponse.getMerchantCode());
    Assertions.assertEquals(UPDATED_BY, merchantConfigurationHistoryWebResponse.getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationHistoryWebResponse.getCreatedBy());
    Assertions.assertEquals(NEUTRAL_STATUS, merchantConfigurationHistoryWebResponse.getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, merchantConfigurationHistoryWebResponse.getNewValue());
    Assertions.assertEquals(ACTIVITY_REGISTERED, merchantConfigurationHistoryWebResponse.getActivity());
    Assertions.assertEquals(Constants.ACTIVITY_REGISTERED_IN, merchantConfigurationHistoryWebResponse.getActivityIn());
  }

  @Test
  public void toMerchantConfigurationHistoryWebResponseTest_activityUpdate() {
    merchantConfigurationHistoryResponse.setActivity(ACTIVITY_UPDATE);
    MerchantConfigurationHistoryWebResponse merchantConfigurationHistoryWebResponse =
        ResponseHelper.toMerchantConfigurationHistoryWebResponse(merchantConfigurationHistoryResponse);
    Assertions.assertNotNull(merchantConfigurationHistoryWebResponse.getCreatedDate());
    Assertions.assertNotNull(merchantConfigurationHistoryWebResponse.getUpdatedDate());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantConfigurationHistoryWebResponse.getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantConfigurationHistoryWebResponse.getMerchantCode());
    Assertions.assertEquals(UPDATED_BY, merchantConfigurationHistoryWebResponse.getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationHistoryWebResponse.getCreatedBy());
    Assertions.assertEquals(NEUTRAL_STATUS, merchantConfigurationHistoryWebResponse.getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, merchantConfigurationHistoryWebResponse.getNewValue());
    Assertions.assertEquals(ACTIVITY_UPDATE, merchantConfigurationHistoryWebResponse.getActivity());
    Assertions.assertEquals(Constants.ACTIVITY_UPDATE_IN, merchantConfigurationHistoryWebResponse.getActivityIn());
  }

  @Test
  public void toMapResponseTest() {
    MapWebResponse response = ResponseHelper.toMapWebResponse(productCollectionCountRestResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getMap());
    Assertions.assertNotNull(response.getMap().get(DAYS_ADDED));
    Map<String, Object> map = (Map<String, Object>) response.getMap().get(DAYS_ADDED);
    Assertions.assertEquals(1L, map.get(TODAY));
    Assertions.assertEquals(2L, map.get(YESTERDAY));
    Assertions.assertEquals(3L, map.get(TWO_DAYS_AGO));
    Assertions.assertEquals(4L, map.get(BETWEEN_3_UNTIL_5_DAYS_OLD));
    Assertions.assertEquals(5L, map.get(MORE_THAN_5_DAYS));
  }

  @Test
  public void toMapResponseTest_forMapResponse() {
    MapWebResponse response = ResponseHelper.toMapWebResponse(mapResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getMap());
    Assertions.assertNotNull(response.getMap().get(DAYS_ADDED));
    Map<String, Object> map = (Map<String, Object>) response.getMap().get(DAYS_ADDED);
    Assertions.assertEquals(1L, map.get(TODAY));
    Map<String, Object> map1 = (Map<String, Object>) response.getMap().get(STATUS_KEY);
    Assertions.assertEquals(2L, map.get(ACTIVE));
  }

  @Test
  public void toLookupResponseList() {
    List<LookupWebResponse> result = ResponseHelper.toLookupWebResponseList(lookupList);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(DESCRIPTION, result.get(0).getDescription());
    Assertions.assertEquals(CODE, result.get(0).getCode());
    Assertions.assertEquals(ID, result.get(0).getId());
    Assertions.assertEquals(LOOKUP_GROUP, result.get(0).getLookupGroup());
    Assertions.assertEquals(NAME, result.get(0).getName());
    Assertions.assertEquals(1, (int) result.get(0).getOrderNumber());
  }

  @Test
  public void toProductCollectionWebResponseListTest() {
    List<ProductCollectionWebResponse> responses =
        ResponseHelper.toProductCollectionWebResponseList(productCollectionResponseList);
    assertNotNull(responses);
    assertEquals(PRODUCT_NAME, responses.get(0).getProductName());
    assertEquals(PRODUCT_CODE, responses.get(0).getProductCode());
    assertEquals(BUSINESS_PARTNER_CODE, responses.get(0).getBusinessPartnerCode());
  }

  @Test
  public void toProductImagePredictionWebResponseTest() {
    ProductImagePredictionWebResponse response =
        ResponseHelper.toProductImagePredictionWebResponse(productImagePredictionResponse);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(DISPLAY_NAME, response.getDisplayName());
    Assertions.assertEquals(PREDICTION_TYPE, response.getPredictionType());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.getConfidenceThreshold());
    Assertions.assertEquals(false,response.isRuleEnabled());
    Assertions.assertEquals(null,response.getRuleType());
    Assertions.assertEquals(0, response.getRuleThreshold());
  }

  @Test
  public void toProductImagePredictionWebResponseRuleTypeAutoNeedRevisionTest() {
    productImagePredictionResponse.setNeedRevisionEnabled(true);
    ProductImagePredictionWebResponse response =
        ResponseHelper.toProductImagePredictionWebResponse(productImagePredictionResponse);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(DISPLAY_NAME, response.getDisplayName());
    Assertions.assertEquals(PREDICTION_TYPE, response.getPredictionType());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.getConfidenceThreshold());
    Assertions.assertEquals(AUTO_NEED_REVISION, response.getRuleType());
    Assertions.assertEquals(true, response.isRuleEnabled());
    Assertions.assertEquals(NEED_REVISION_CONFIENCE_THERSHOLD, response.getRuleThreshold());
  }

  @Test
  public void toProductImagePredictionWebResponseRuleTypeForceReviewTest() {
    productImagePredictionResponse.setForceReview(true);
    ProductImagePredictionWebResponse response =
        ResponseHelper.toProductImagePredictionWebResponse(productImagePredictionResponse);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(DISPLAY_NAME, response.getDisplayName());
    Assertions.assertEquals(PREDICTION_TYPE, response.getPredictionType());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.getConfidenceThreshold());
    Assertions.assertEquals(FORCE_REVIEW, response.getRuleType());
    Assertions.assertEquals(true, response.isRuleEnabled());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.getRuleThreshold());
  }

  @Test
  public void toListOfProductImagePredictionWebResponse() {
    List<ProductImagePredictionWebResponse> response =
        ResponseHelper.toListOfProductImagePredictionWebResponse(productImagePredictionResponseList);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(productImagePredictionResponseList.size(), response.size());
    Assertions.assertEquals(PREDICTION_TYPE, response.get(0).getPredictionType());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.get(0).getConfidenceThreshold());
  }

  @Test
  public void toVendorDetailWebResponseListTest() {
    List<VendorDetailWebResponse> responses = ResponseHelper.toVendorDetailWebResponseList(vendorCapacityDTOList);
    assertNotNull(responses);
    assertEquals(1, responses.size());
    assertEquals(NAME, responses.get(0).getName());
    assertEquals(ID, responses.get(0).getId());
    assertEquals(VENDOR_CODE, responses.get(0).getVendorCode());
    assertEquals(100000L, responses.get(0).getRemainingCapacity().longValue());
  }

  @Test
  public void toVendorDetailWebResponseListWithEmptyListTest() {
    vendorCapacityDTOList.clear();
    List<VendorDetailWebResponse> responses = ResponseHelper.toVendorDetailWebResponseList(vendorCapacityDTOList);
    assertNotNull(responses);
    assertEquals(0, responses.size());
  }

  @Test
  public void toProductImageFeedbackResponseTest() throws IOException {
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, true);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(2, response.getImageFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getUserFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().get(1).getSystemFeedback().size());
    Assertions.assertEquals(PATH_1, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(PATH_2, response.getImageFeedback().get(1).getLocationPath());
    Assertions.assertEquals(WATERMARK_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(1));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(1).getSystemFeedback().get(0));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(0).getUserFeedback().get(0));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(1).getUserFeedback().get(0));
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getUserFeedback().get(1));
  }

  @Test
  public void toProductImageFeedbackResponseuserFeedbackNullTest() throws IOException {
    productImageQcFeedbackResponse.setUserFeedback(USER_FEEDBACK_2);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(2, response.getImageFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().get(1).getSystemFeedback().size());
    Assertions.assertEquals(PATH_1, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(PATH_2, response.getImageFeedback().get(1).getLocationPath());
    Assertions.assertEquals(WATERMARK_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(1));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(1).getSystemFeedback().get(0));
  }

  @Test
  public void toProductImageFeedbackResponseEditedTest() throws IOException {
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK_WITH_EDITED_FLAG);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertFalse(response.getImageFeedback().get(0).isEdited());
    Assertions.assertFalse(response.getImageFeedback().get(1).isEdited());
    Assertions.assertTrue(response.getImageFeedback().get(2).isEdited());
    Assertions.assertTrue(response.getImageFeedback().get(3).isEdited());
  }

  @Test
  public void toProductImageFeedbackResponseEditedTest1() throws IOException {
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK_WITH_EDITED_FLAG_1);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertFalse(response.getImageFeedback().get(0).isEdited());
    Assertions.assertFalse(response.getImageFeedback().get(1).isEdited());
    Assertions.assertTrue(response.getImageFeedback().get(2).isEdited());
    Assertions.assertTrue(response.getImageFeedback().get(3).isEdited());
  }

  @Test
  public void toProductImageFeedbackResponseUserFeedbackNullTest() throws IOException {
    productImageQcFeedbackResponse.setUserFeedback(null);
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBCK_1);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(1, response.getImageFeedback().size());
    Assertions.assertTrue(CollectionUtils.isEmpty(response.getImageFeedback().get(0).getUserFeedback()));
    Assertions.assertEquals(1, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(IMAGE_FILENAME, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getSystemFeedback().get(0));
  }

  @Test
  public void toProductImageFeedbackResponseUserFeedbackTest() throws IOException {
    productImageQcFeedbackResponse.setUserFeedback(USER_FEEDBACK_1);
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBCK_1);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(1, response.getImageFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(IMAGE_FILENAME, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getUserFeedback().get(0));
  }

  @Test
  public void toProductImageFeedbackResponseEmptySystemFeedbackTest() throws IOException {
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK_GOOD_IMAGE);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(2, response.getImageFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getUserFeedback().size());
    Assertions.assertEquals(1, response.getImageFeedback().get(1).getSystemFeedback().size());
    Assertions.assertEquals(PATH_1, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(PATH_2, response.getImageFeedback().get(1).getLocationPath());
    Assertions.assertEquals(WATERMARK_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(1));
    Assertions.assertEquals(ImageQcConstants.GOOD_EN, response.getImageFeedback().get(1).getSystemFeedback().get(0));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(0).getUserFeedback().get(0));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(1).getUserFeedback().get(0));
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getUserFeedback().get(1));
  }

  @Test
  public void toProductImageFeedbackResponseEmptySystemFeedbackMFDTrueTest() throws IOException {
    productImageQcFeedbackResponse.setSystemFeedback(SYSTEM_FEEDBACK_GOOD_IMAGE_1);
    ProductImageQcWebResponse response =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse, new HashMap<>(), new HashMap<>(), GCS_URL_PREFIX, false);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(1, response.getImageFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getSystemFeedback().size());
    Assertions.assertEquals(2, response.getImageFeedback().get(0).getUserFeedback().size());
    Assertions.assertEquals(PATH_1, response.getImageFeedback().get(0).getLocationPath());
    Assertions.assertEquals(WATERMARK_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(0));
    Assertions.assertEquals(NSFW_PRESENT, response.getImageFeedback().get(0).getSystemFeedback().get(1));
    Assertions.assertEquals(TEXT, response.getImageFeedback().get(0).getUserFeedback().get(0));
    Assertions.assertEquals(BLUR, response.getImageFeedback().get(0).getUserFeedback().get(1));
  }

  @Test
  public void toFaultyTypeListWebResponseTest() {
    List<ImageFaultyTypeWebResponse> response = ResponseHelper
        .toFaultyTypeListWebResponse(predictionTypeListResponse, IGNORE_IMAGE_QC, DONT_IGNORE_IMAGE_QC,
            new HashMap<>(), true);
    Assertions.assertEquals(9, response.size());
    Assertions.assertEquals(TEXT, response.get(0).getEnName());
    Assertions.assertTrue(response.get(0).getIgnoreForImageQc());
    Assertions.assertEquals(TEXT_IN, response.get(0).getInName());
    Assertions.assertEquals(BLUR, response.get(1).getEnName());
    Assertions.assertFalse(response.get(1).getIgnoreForImageQc());
    Assertions.assertEquals(BLUR_IN, response.get(1).getInName());
    Assertions.assertEquals(NSFW_PRESENT, response.get(2).getEnName());
    Assertions.assertEquals(NSFW_PRESENT_IN, response.get(2).getInName());
    Assertions.assertEquals(WATERMARK_PRESENT, response.get(3).getEnName());
    Assertions.assertEquals(WATERMARK_PRESENT_IN, response.get(3).getInName());
    Assertions.assertEquals(ImageQcConstants.BRAND_EN, response.get(6).getEnName());
    Assertions.assertEquals(ImageQcConstants.BRAND_IN, response.get(6).getInName());
    Assertions.assertTrue(response.stream().allMatch(
        imageFaultyTypeWebResponse -> DEFAULT_LABEL_COLOUR.equals(imageFaultyTypeWebResponse.getLabelColour())));
  }

  @Test
  public void toFaultyTypeListWebResponse1Test() {
    HashMap<String, String> labelColourMap = new HashMap<>();
    labelColourMap.putIfAbsent(TEXT, DANGER_LABEL_COLOUR);
    labelColourMap.putIfAbsent(BLUR, WARNING_LABEL_COLOUR);
    List<ImageFaultyTypeWebResponse> response = ResponseHelper
        .toFaultyTypeListWebResponse(predictionTypeListResponse, IGNORE_IMAGE_QC, DONT_IGNORE_IMAGE_QC, labelColourMap,
          false);
    Assertions.assertEquals(8, response.size());
    Assertions.assertEquals(TEXT, response.get(0).getEnName());
    Assertions.assertEquals(DANGER_LABEL_COLOUR, response.get(0).getLabelColour());
    Assertions.assertTrue(response.get(0).getIgnoreForImageQc());
    Assertions.assertEquals(TEXT_IN, response.get(0).getInName());
    Assertions.assertEquals(BLUR, response.get(1).getEnName());
    Assertions.assertEquals(WARNING_LABEL_COLOUR, response.get(1).getLabelColour());
    Assertions.assertEquals(BLUR_IN, response.get(1).getInName());
    Assertions.assertFalse(response.get(1).getIgnoreForImageQc());
    Assertions.assertEquals(NSFW_PRESENT, response.get(2).getEnName());
    Assertions.assertEquals(NSFW_PRESENT_IN, response.get(2).getInName());
    Assertions.assertEquals(WATERMARK_PRESENT, response.get(3).getEnName());
    Assertions.assertEquals(WATERMARK_PRESENT_IN, response.get(3).getInName());
    Assertions.assertEquals(ImageQcConstants.BRAND_EN, response.get(6).getEnName());
    Assertions.assertEquals(ImageQcConstants.BRAND_IN, response.get(6).getInName());
  }

  @Test
  public void validateProductStateExceptionTest() {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setActivated(true);
    try {
      ResponseHelper.validateProductState(
          new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID));
    } catch (InvalidStateException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void validateProductStateTest() {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    ResponseHelper.validateProductState(new GdnRestSingleResponse<>(productDetailCompleteResponse, REQUEST_ID));
  }

  @Test
  public void toHistoryWebResponseListFromProductHistoryResponseTest() {
    List<HistoryWebResponse> responses = ResponseHelper
        .toHistoryWebResponseListFromProductHistoryResponse(productHistoryResponseList);
    assertNotNull(responses);
    assertEquals(1, responses.size());
    assertEquals(PRODUCT_ID, responses.get(0).getProductId());
    assertEquals(DESCRIPTION, responses.get(0).getActivity());
    assertEquals(NOTES, responses.get(0).getDescription());
    assertEquals(UPDATED_BY, responses.get(0).getUpdatedBy());
    assertEquals(CREATED_BY, responses.get(0).getCreatedBy());
  }

  @Test
  public void toHistoryWebResponseListFromTaskHistoryResponseTest() {
    List<HistoryWebResponse> responses =
        ResponseHelper.toHistoryWebResponseListFromTaskHistoryResponse(taskHistoryResponseList);
    assertNotNull(responses);
    assertEquals(1, responses.size());
    assertEquals(PRODUCT_CODE, responses.get(0).getProductCode());
    assertEquals(NOTES_ASSERTION, responses.get(0).getReason());
    assertEquals(DESCRIPTION, responses.get(0).getActivity());
    assertEquals(NOTES_ASSERTION, responses.get(0).getDescription());
  }

  @Test
  public void checkCategoryChangeTest_differentWholesaleConfig() {
    boolean response = ResponseHelper.categoryWholesaleConfigCheck(wholesaleMappingResponse, wholesaleMappingResponse2);
    assertFalse(response);
  }

  @Test
  public void checkCategoryChangeTest_differentWholesaleConfigType() {
    wholesaleMappingResponse2.setConfigurationType(StringUtils.EMPTY);
    boolean response = ResponseHelper.categoryWholesaleConfigCheck(wholesaleMappingResponse, wholesaleMappingResponse2);
    assertFalse(response);
  }

  @Test
  public void checkCategoryChangeTest_differentWholesaleConfigMinDiscount() {
    wholesaleMappingResponse2.getWholesaleConfig().get(0).getMinWholesaleDiscount().get(0).setPrice(0.0);
    boolean response = ResponseHelper.categoryWholesaleConfigCheck(wholesaleMappingResponse, wholesaleMappingResponse2);
    assertFalse(response);
  }

  @Test
  public void toListProductCenterSummaryWebResponseTest() {
    ProductCenterSummaryResponse productCenterSummaryResponse1 = new ProductCenterSummaryResponse();
    productCenterSummaryResponse1.setProductSku(SKU_CODE);
    productCenterSummaryResponse1.setProductName(PRODUCT_NAME);
    productCenterSummaryResponse1.setMasterCategory(new MasterCategoryResponse(CATEGORY_CODE, CATEGORY_NAME));
    productCenterSummaryResponse1.setSalesCategories(Arrays.asList(CATEGORY_CODE));

    ProductCenterSummaryResponse productCenterSummaryResponse2 = new ProductCenterSummaryResponse();
    productCenterSummaryResponse2.setProductSku(SKU_CODE);
    productCenterSummaryResponse2.setProductName(PRODUCT_NAME);
    productCenterSummaryResponse2.setMasterCategory(new MasterCategoryResponse(CATEGORY_CODE, CATEGORY_NAME));
    productCenterSummaryResponse2.setSalesCategories(Arrays.asList(CATEGORY_CODE));

    List<ProductCenterSummaryWebResponse> response = ResponseHelper.toListProductCenterSummaryWebResponse(Arrays.asList(productCenterSummaryResponse1, productCenterSummaryResponse2));

    Assertions.assertEquals(2, response.size());
    Assertions.assertEquals(SKU_CODE, response.get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_NAME, response.get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getMasterCategory().getCategoryCode());
  }

  @Test
  public void toProductCenterHistoryWebResponseTest() {
    ProductCenterHistoryWebResponse productCenterHistoryWebResponse =
        ResponseHelper.toProductCenterHistoryWebResponse(productCenterHistoryResponse);
    Assertions.assertEquals(PRODUCT_CENTER_ACTIVITY, productCenterHistoryWebResponse.getActivity());
    Assertions.assertEquals(PRODUCT_SKU, productCenterHistoryWebResponse.getProductSku());
    Assertions.assertEquals(PRODUCT_CENTER_DESCRIPTION, productCenterHistoryWebResponse.getDescription());
    Assertions.assertEquals(PRODUCT_CENTER_UPDATEDBY, productCenterHistoryWebResponse.getUser());
  }

  @Test
  public void toProductDetailWebResponseWithPreOrderDetailsTest() {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    productDetailCompleteResponse.setPreOrder(preOrderResponse);
    productDetailCompleteResponse.setSizeChartCode(SIZE_CHART_CODE);
    productDetailCompleteResponse.setSizeChartName(SIZE_CHART_NAME);
    productDetailCompleteResponse.setRestrictedKeywordsDetected(
        Arrays.asList(new RestrictedKeywordsByFieldResponse("PRODUCT_NAME", Arrays.asList("abc@gmail.com"))));
    ProductDetailWebResponse response =
        ResponseHelper.toProductDetailWebResponseFromProductDetailCompleteResponse(productDetailCompleteResponse, profileResponse);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    assertEquals(ATTRIBUTE_ID, response.getProductAttributeResponses().get(0).getId());
    assertEquals(1, response.getImages().size());
    assertEquals(1, response.getCategories().size());
    assertEquals(1, response.getCategoriesEnglish().size());
    assertNotNull(response.getProductCategoryResponses());
    assertEquals(CODE, response.getProductCategoryResponses().get(0).getCategoryCode());
    assertEquals(2, response.getProductItemResponses().size());
    List<ProductItemWebResponse> productItemWebResponses =
        new ArrayList<ProductItemWebResponse>(response.getProductItemResponses());
    List<ProductCategoryWebResponse> productCategoryWebResponses =
        new ArrayList<>(response.getProductCategoryResponses());
    assertEquals(NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(NAME1, productItemWebResponses.get(1).getGeneratedItemName());
    assertTrue(response.isPostLive());
    assertTrue(response.isReviewPending());
    assertTrue(productCategoryWebResponses.get(0).isWholesalePriceConfigEnabled());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertTrue(response.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, response.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, response.getPreOrder().getPreOrderValue());
    assertEquals("PRODUCT_NAME", response.getRestrictedKeywordsDetected().get(0).getFieldIdentifier());
    assertEquals("abc@gmail.com", response.getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
    assertEquals(SIZE_CHART_CODE, response.getSizeChartCode());
    assertEquals(SIZE_CHART_NAME, response.getSizeChartName());
  }

  @Test
  public void toProductDetailWebResponseWithPreOrderDetailsNullTest() {
    BeanUtils.copyProperties(productDetailResponse, productDetailCompleteResponse);
    ProductDetailWebResponse response =
        ResponseHelper.toProductDetailWebResponseFromProductDetailCompleteResponse(productDetailCompleteResponse, profileResponse);
    assertNotNull(response);
    assertNotNull(response.getProductAttributeResponses());
    assertEquals(ATTRIBUTE_ID, response.getProductAttributeResponses().get(0).getId());
    assertEquals(1, response.getImages().size());
    assertEquals(1, response.getCategories().size());
    assertEquals(1, response.getCategoriesEnglish().size());
    assertNotNull(response.getProductCategoryResponses());
    assertEquals(CODE, response.getProductCategoryResponses().get(0).getCategoryCode());
    assertEquals(2, response.getProductItemResponses().size());
    List<ProductItemWebResponse> productItemWebResponses =
        new ArrayList<ProductItemWebResponse>(response.getProductItemResponses());
    List<ProductCategoryWebResponse> productCategoryWebResponses =
        new ArrayList<>(response.getProductCategoryResponses());
    assertEquals(NAME, productItemWebResponses.get(0).getGeneratedItemName());
    assertEquals(NAME1, productItemWebResponses.get(1).getGeneratedItemName());
    assertTrue(response.isPostLive());
    assertTrue(response.isReviewPending());
    assertTrue(productCategoryWebResponses.get(0).isWholesalePriceConfigEnabled());
    assertFalse(response.isInternationalFlag());
    assertEquals(CC, response.getCommissionType());
    assertNotNull(response.getPreOrder());
    assertFalse(response.getPreOrder().getIsPreOrder());
  }

  @Test
  public void toRecatProductSummaryWebResponseTest() {
    List<RecatProductSummaryWebResponse> productSummaryWebResponses =
        ResponseHelper.toRecatProductSummaryWebResponse(Arrays.asList(recatProductSummaryResponse));
    Assertions.assertEquals(RECAT_REQUEST_CODE, productSummaryWebResponses.get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, productSummaryWebResponses.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSummaryWebResponses.get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, productSummaryWebResponses.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, productSummaryWebResponses.get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, productSummaryWebResponses.get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, productSummaryWebResponses.get(0).getNewCategoryName());
    Assertions.assertEquals("IN_PROGRESS", productSummaryWebResponses.get(0).getStatus());
    Assertions.assertEquals("Succeed", productSummaryWebResponses.get(0).getNotes());
    Assertions.assertNotNull(productSummaryWebResponses.get(0).getUpdatedDate());
  }

  @Test
  public void toRecatProductSummaryWebResponseTest2() {
    recatProductSummaryResponse.setStatus("FINISHED");
    List<RecatProductSummaryWebResponse> productSummaryWebResponses =
        ResponseHelper.toRecatProductSummaryWebResponse(Arrays.asList(recatProductSummaryResponse));
    Assertions.assertEquals(RECAT_REQUEST_CODE, productSummaryWebResponses.get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, productSummaryWebResponses.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productSummaryWebResponses.get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, productSummaryWebResponses.get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, productSummaryWebResponses.get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, productSummaryWebResponses.get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, productSummaryWebResponses.get(0).getNewCategoryName());
    Assertions.assertEquals("FINISHED", productSummaryWebResponses.get(0).getStatus());
    Assertions.assertEquals("Succeed", productSummaryWebResponses.get(0).getNotes());
    Assertions.assertNotNull(productSummaryWebResponses.get(0).getUpdatedDate());
  }

  @Test
  public void toRecatProductCountWebResponseTest() {
    RecatProductCountResponse recatProductCountResponse =
        new RecatProductCountResponse(20, 10, 0, 10, "PARTIAL_SUCCESS");
    RecatProductCountWebResponse recatProductCountWebResponse =
        ResponseHelper.toRecatProductCountWebResponse(recatProductCountResponse);
    Assertions.assertEquals(20L, recatProductCountWebResponse.getTotalCount());
    Assertions.assertEquals(10L, recatProductCountWebResponse.getSuccessCount());
    Assertions.assertEquals(0L, recatProductCountWebResponse.getInProgressCount());
    Assertions.assertEquals(10L, recatProductCountWebResponse.getFailedCount());
    Assertions.assertEquals("PARTIAL_SUCCESS", recatProductCountWebResponse.getStatus());
  }

  @Test
  public void toRecatProcessSummaryWebResponsePageTest() {
    Page<RecatProcessSummaryWebResponse> response = ResponseHelper
        .toRecatProcessSummaryWebResponsePage(
            new PageImpl<>(Arrays.asList(recatProcessSummaryResponse), PageRequest.of(0, 10), 1));
    Assertions.assertEquals(RECAT_STATUS, response.getContent().get(0).getStatus());
    Assertions.assertEquals(CREATED_BY, response.getContent().get(0).getInitiator());
    Assertions.assertEquals(10, response.getContent().get(0).getProductCount());
  }

  @Test
  public void toBulkInternalProcessSummaryWebResponseTest() {
    List<BulkInternalProcessSummaryResponse> bulkInternalProcessSummaryResponseList = new ArrayList<>();
    BulkInternalProcessSummaryResponse bulkInternalProcessSummaryResponse = new BulkInternalProcessSummaryResponse();
    bulkInternalProcessSummaryResponse.setStatus(STATUS);
    bulkInternalProcessSummaryResponse.setInternalProcessRequestCode(INTERNAL_PROCESS_REQUEST_CODE);
    bulkInternalProcessSummaryResponseList.add(bulkInternalProcessSummaryResponse);
    List<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummaryWebResponseList =
        ResponseHelper.toBulkInternalProcessSummaryWebResponse(bulkInternalProcessSummaryResponseList);
    Assertions.assertEquals(STATUS, bulkInternalProcessSummaryWebResponseList.get(0).getStatus());
    Assertions.assertEquals(INTERNAL_PROCESS_REQUEST_CODE,
        bulkInternalProcessSummaryWebResponseList.get(0).getInternalProcessRequestCode());
  }

  @Test
  public void toBrandAuthFilterWebResponseTest() {
    HashMap<String, ProfileResponse> responseHashMap = new HashMap<>();
    ProfileResponse response =
      ProfileResponse.builder().company(CompanyDTO.builder().name(DEFAULT_NAME).build()).build();
    responseHashMap.put(SELLER_CODE, response);
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setSellerCode(SELLER_CODE);
    brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    brandAuthFilterResponse.setAuthEndDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthFilterResponse.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    List<BrandAuthFilterWebResponse> brandAuthFilterWebResponses =
      ResponseHelper.toBrandAuthFilterWebResponseList(List.of(brandAuthFilterResponse),
        responseHashMap, true);
    Assertions.assertNotNull(brandAuthFilterResponse);
    Assertions.assertEquals(1, brandAuthFilterWebResponses.size());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterWebResponses.get(0).getSellerCode());
    Assertions.assertEquals(BrandAuthorisationStatus.ACTIVE.name(), brandAuthFilterWebResponses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterWebResponseTest_underReviewTab() {
    HashMap<String, ProfileResponse> responseHashMap = new HashMap<>();
    ProfileResponse response =
        ProfileResponse.builder().company(CompanyDTO.builder().name(DEFAULT_NAME).build()).build();
    responseHashMap.put(SELLER_CODE, response);
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setSellerCode(SELLER_CODE);
    brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    brandAuthFilterResponse.setAuthEndDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthFilterResponse.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    List<BrandAuthFilterWebResponse> brandAuthFilterWebResponses =
        ResponseHelper.toBrandAuthFilterWebResponseList(List.of(brandAuthFilterResponse),
            responseHashMap, false);
    Assertions.assertNotNull(brandAuthFilterResponse);
    Assertions.assertEquals(1, brandAuthFilterWebResponses.size());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterWebResponses.get(0).getSellerCode());
    Assertions.assertEquals(BrandAuthorisationStatus.ACTIVE.name(), brandAuthFilterWebResponses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterWebResponseActiveWithWrongDateRangeTest() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setSellerCode(SELLER_CODE);
    brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    brandAuthFilterResponse.setAuthEndDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    brandAuthFilterResponse.setAuthStartDate(Date.from(Instant.now().minus(Duration.ofDays(1))));
    List<BrandAuthFilterWebResponse> brandAuthFilterWebResponses =
        ResponseHelper.toBrandAuthFilterWebResponseList(Arrays.asList(brandAuthFilterResponse),
          new HashMap<>(), true);
    Assertions.assertNotNull(brandAuthFilterResponse);
    Assertions.assertEquals(1, brandAuthFilterWebResponses.size());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterWebResponses.get(0).getSellerCode());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(), brandAuthFilterWebResponses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterWebResponseActiveWithWrongDateRange1Test() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setSellerCode(SELLER_CODE);
    brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.ACTIVE.name());
    brandAuthFilterResponse.setAuthEndDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthFilterResponse.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    List<BrandAuthFilterWebResponse> brandAuthFilterWebResponses =
        ResponseHelper.toBrandAuthFilterWebResponseList(Arrays.asList(brandAuthFilterResponse),
          new HashMap<>(), true);
    Assertions.assertNotNull(brandAuthFilterResponse);
    Assertions.assertEquals(1, brandAuthFilterWebResponses.size());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterWebResponses.get(0).getSellerCode());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(), brandAuthFilterWebResponses.get(0).getStatus());
  }

  @Test
  public void toBrandAuthFilterWebResponseActiveWithInactiveTest() {
    BrandAuthFilterResponse brandAuthFilterResponse = new BrandAuthFilterResponse();
    brandAuthFilterResponse.setSellerCode(SELLER_CODE);
    brandAuthFilterResponse.setStatus(BrandAuthorisationStatus.INACTIVE.name());
    brandAuthFilterResponse.setAuthEndDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    brandAuthFilterResponse.setAuthStartDate(Date.from(Instant.now().plus(Duration.ofDays(1))));
    List<BrandAuthFilterWebResponse> brandAuthFilterWebResponses =
        ResponseHelper.toBrandAuthFilterWebResponseList(Arrays.asList(brandAuthFilterResponse),
          new HashMap<>(), true);
    Assertions.assertNotNull(brandAuthFilterResponse);
    Assertions.assertEquals(1, brandAuthFilterWebResponses.size());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterWebResponses.get(0).getSellerCode());
    Assertions.assertEquals(BrandAuthorisationStatus.INACTIVE.name(), brandAuthFilterWebResponses.get(0).getStatus());
  }

  @Test
  public void toProductImagePredictionAndCategoryMappingResponseTest() {
    List<ProductImagePredictionAndCategoryMappingResponse> productImagePredictionAndCategoryMappingResponseList =
        new ArrayList<>();
    List<CategoryCodeAndCategoryNameResponse> categoryCodeAndCategoryNameResponseList = new ArrayList<>();
    CategoryCodeAndCategoryNameResponse categoryCodeAndCategoryNameResponse = new CategoryCodeAndCategoryNameResponse();
    categoryCodeAndCategoryNameResponse.setCategoryCode(CATEGORY_CODE);
    categoryCodeAndCategoryNameResponse.setCategoryName(CATEGORY_NAME);
    categoryCodeAndCategoryNameResponseList.add(categoryCodeAndCategoryNameResponse);
    ProductImagePredictionAndCategoryMappingResponse productImagePredictionAndCategoryMappingResponse =
        new ProductImagePredictionAndCategoryMappingResponse();
    productImagePredictionAndCategoryMappingResponse.setPredictionType(PREDICTION_TYPE);
    productImagePredictionAndCategoryMappingResponse.setRuleEnabled(RULE_ENABLED);
    productImagePredictionAndCategoryMappingResponse.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponse.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponse.setCategoryCodeAndCategoryNameResponseList(
        categoryCodeAndCategoryNameResponseList);
    ProductImagePredictionAndCategoryMappingResponse productImagePredictionAndCategoryMappingResponse1 =
        new ProductImagePredictionAndCategoryMappingResponse();
    productImagePredictionAndCategoryMappingResponse1.setPredictionType(PREDICTION_TYPE);
    productImagePredictionAndCategoryMappingResponse1.setRuleEnabled(RULE_ENABLED);
    productImagePredictionAndCategoryMappingResponse1.setConfidenceThreshold(CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponse1.setTextConfidenceThreshold(TEXT_CONFIDENCE_THRESHOLD);
    productImagePredictionAndCategoryMappingResponseList.add(productImagePredictionAndCategoryMappingResponse);
    productImagePredictionAndCategoryMappingResponseList.add(productImagePredictionAndCategoryMappingResponse1);
    List<ProductImagePredictionAndCategoryMappingWebResponse> response =
        ResponseHelper.toProductImagePredictionAndCategoryMappingResponse(
            productImagePredictionAndCategoryMappingResponseList);
    Assertions.assertEquals(2, response.size());
    Assertions.assertEquals(PREDICTION_TYPE, response.get(0).getPredictionType());
    Assertions.assertEquals(RULE_ENABLED, response.get(0).isRuleEnabled());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.get(0).getConfidenceThreshold());
    Assertions.assertEquals(TEXT_CONFIDENCE_THRESHOLD, response.get(0).getTextConfidenceThreshold());
    Assertions.assertEquals(CATEGORY_CODE,
        response.get(0).getPredictionCategoryMappingWebResponseList().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
        response.get(0).getPredictionCategoryMappingWebResponseList().get(0).getCategoryName());
    Assertions.assertEquals(PREDICTION_TYPE, response.get(1).getPredictionType());
    Assertions.assertEquals(RULE_ENABLED, response.get(1).isRuleEnabled());
    Assertions.assertEquals(CONFIDENCE_THRESHOLD, response.get(1).getConfidenceThreshold());
    Assertions.assertEquals(TEXT_CONFIDENCE_THRESHOLD, response.get(1).getTextConfidenceThreshold());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndBrandModelTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"https://storage.googleapis.com/merchant-prod-image-static/source-image/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}],\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{\"otherModelFeedBack\":[\"Prohibited drugs\", \"Brand mismatch\"]}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
            ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
            ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, false);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getBrandModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getBrandModels().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertEquals("Prohibited drugs",
        productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().iterator().next());
    Assertions.assertEquals("Brand mismatch",
        productImageQcWebResponse.getBrandModels().getUserFeedback().iterator().next());
    Assertions.assertFalse(productImageQcWebResponse.getImageFeedback().get(0).getLocationPath().contains(GCS_URL_PREFIX));
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndBrandModelUserFeedbackNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}],\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
        ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, false);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getBrandModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getBrandModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getBrandModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelMismatchCombinedNotEmptyUserFeedbackNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[], \"categoryModels\":[{\"predictionType\":\"category_mismatch\",\"mismatchCombined\":true}] ,\"brandModels\":[]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("category_mismatch", "Category mismatch"),
        ImmutableMap.of("category_mismatch", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(0, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelMismatchCombinedTrueUserFeedbackNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}], \"categoryModels\":[{\"predictionType\":\"category_mismatch\",\"mismatchCombined\":true}] ,\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("category_mismatch", "Category mismatch"),
        ImmutableMap.of("category_mismatch", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelMismatchCombinedFalseUserFeedbackNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[], \"categoryModels\":[{\"predictionType\":\"category_mismatch\",\"mismatchCombined\":false}] ,\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("category_mismatch", "Category mismatch"),
        ImmutableMap.of("category_mismatch", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(0, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelMismatchCombinedUserFeedbackNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[], \"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("category_mismatch", "Category mismatch"),
        ImmutableMap.of("category_mismatch", 70.0), GCS_URL_PREFIX, false);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelKeyNotPresentTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}], \"categoryModels\":[{\"predictionType\":\"category_mismatch\"}] ,\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
        ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(0, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndMismatchCombinedFalseTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}], \"categoryModels\":[{\"predictionType\":\"category_mismatch\",\"mismatchCombined\":false}] ,\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
        ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(0, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getCategoryModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseRestrictiveAndCategoryModelTest() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"https://storage.googleapis.com/merchant-prod-image-static/source-image/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":true,\"confidence\":0}]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":true,\"confidence\":100}]}],\"categoryModels\":[{\"predictionType\":\"category_mismatch\", \"mismatchCombined\":true}],\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[{\"brand\":null,\"confidence\":99},{\"brand\":null,\"confidence\":0}]},{\"predictionType\":\"protected_brand_predictions\",\"predictions\":[{\"brand\":\"NIKE\",\"confidence\":0},{\"brand\":\"adidas\",\"confidence\":0},{\"brand\":\"Vans\",\"confidence\":0},{\"brand\":\"Asus\",\"confidence\":0},{\"brand\":\"Converse\",\"confidence\":0},{\"brand\":\"Samsung\",\"confidence\":0},{\"brand\":\"HP\",\"confidence\":80},{\"brand\":\"Apple\",\"confidence\":0}]}]}";
    String userFeedback = "{\"otherModelFeedBack\":[\"Prohibited drugs\", \"Category mismatch\"]}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
            ImmutableMap.of("category_mismatch", "Category mismatch"),
            ImmutableMap.of("category_mismatch", 70.0), GCS_URL_PREFIX, true);
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().isEmpty());
    Assertions.assertEquals(2, productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getCategoryModels().getSystemFeedback().isEmpty());
    Assertions.assertEquals(1, productImageQcWebResponse.getCategoryModels().getSystemFeedback().size());
    Assertions.assertFalse(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertEquals("Prohibited drugs",
        productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().iterator().next());
    Assertions.assertEquals("Category mismatch",
        productImageQcWebResponse.getCategoryModels().getUserFeedback().iterator().next());
    Assertions.assertFalse(productImageQcWebResponse.getImageFeedback().get(0).getLocationPath().contains(GCS_URL_PREFIX));
  }

  @Test
  public void toProductImageFeedbackResponseEmptyPredictionList() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}],\"success\":true,\"errorMessage\":\"\",\"restrictionModels\":[{\"predictionType\":\"pharma_prescription\",\"predictions\":[]},{\"predictionType\":\"pharma_banned\",\"predictions\":[{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}]},{\"predictionType\":\"google_restricted\",\"predictions\":[{\"predictionType\":\"google_restricted\",\"displayName\":\"Google restriction\",\"present\":false,\"confidence\":100}]}],\"brandModels\":[{\"predictionType\":\"brand_recommendation\",\"predictions\":[]}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse = ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
        ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
        ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, false);
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().contains(ImageQcConstants.GOOD_EN));
    Assertions.assertTrue(productImageQcWebResponse.getBrandModels().getSystemFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getBrandModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toProductImageFeedbackResponseNoRestictionAndBrandModel() throws IOException {
    String systemFeedback =
        "{\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-48118269/resize/oppo_diamond_mobile_legends_ml_mlbb_express_-_1050_dm_full01_ufc04x1d.jpg\",\"hashCode\":\"b2e43590b71f7bffab67ea37ff083826\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":2},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":false,\"confidence\":37},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0},{\"predictionType\":\"logo_predictions\",\"displayName\":\"Other e-commerce/social media logos\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_prescription\",\"displayName\":\"Prescription drugs\",\"present\":false,\"confidence\":0},{\"predictionType\":\"pharma_banned\",\"displayName\":\"Prohibited drugs\",\"present\":false,\"confidence\":0}],\"edited\":false,\"markForDelete\":false}]}";
    String userFeedback = "{}";
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse =
        new ProductImageQcFeedbackResponse(PRODUCT_CODE, systemFeedback, userFeedback);
    ProductImageQcWebResponse productImageQcWebResponse =
        ResponseHelper.toProductImageQcWebResponse(productImageQcFeedbackResponse,
            ImmutableMap.of("protected_brand_predictions", "Brand mismatch"),
            ImmutableMap.of("protected_brand_predictions", 70.0), GCS_URL_PREFIX, false);
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getSystemFeedback().contains(ImageQcConstants.GOOD_EN));
    Assertions.assertTrue(productImageQcWebResponse.getBrandModels().getSystemFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getRestrictiveModelFeedback().getUserFeedback().isEmpty());
    Assertions.assertTrue(productImageQcWebResponse.getBrandModels().getUserFeedback().isEmpty());
  }

  @Test
  public void toSetHalalResponseTest() {
    GdnRestListResponse<HalalProductResponse> halalProductResponseList = new GdnRestListResponse<>();
    List<HalalProductResponse> halalProductResponsesList = new ArrayList<>();
    HalalProductResponse halalProductResponse = new HalalProductResponse();
    halalProductResponse.setHalalProduct(true);
    halalProductResponse.setProductName(PRODUCT_NAME);
    halalProductResponse.setProductSku(PRODUCT_SKU);
    halalProductResponse.setProductCode(PRODUCT_CODE);
    halalProductResponsesList.add(halalProductResponse);
    halalProductResponseList.setContent(halalProductResponsesList);
    halalProductResponseList.setSuccess(true);
    ResponseHelper.toSetHalalResponse(halalProductResponsesList,
        new HalalProductWebResponse(), PRODUCT_SKU, PATH_1);
  }

  @Test
  public void validateResponse_BPJPHListResponse_SuccessTrueTest() {
    BPJPHListResponse bpjphListResponse = new BPJPHListResponse(200, null, false, new BPJPHData());
    assertTrue(ResponseHelper.validateResponse(bpjphListResponse));
  }

  @Test
  public void validateResponse_BPJPHListResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((BPJPHListResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_BPJPHListResponse_NullDataTest() {
    Exception exception = new Exception();
    BPJPHListResponse bpjphListResponse = new BPJPHListResponse();
    bpjphListResponse.setError(false);
    bpjphListResponse.setStatusCode(200);
    bpjphListResponse.setData(null);
    try {
      ResponseHelper.validateResponse(bpjphListResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.BPJPH_RESPONSE_FAILED, exception.getMessage());
    }
  }

  @Test
  public void validateResponse_BPJPHListResponse_SuccessFalseTest() {
    BPJPHListResponse bpjphListResponse = new BPJPHListResponse(400, null, true, null);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(bpjphListResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.BPJPH_RESPONSE_FAILED, exception.getMessage());
    }
  }

  @Test
  public void toHalalCertificationDetailResponseTest() {
    BPJPHData bpjphData = new BPJPHData();
    HalalCeritficationDetails halalCeritficationDetails = new HalalCeritficationDetails();
    halalCeritficationDetails.setNo_sert(CERTIFICATION_NUMBER);
    halalCeritficationDetails.setTgl_sert(ISSUED_DATE);
    halalCeritficationDetails.setTgl_valid(EXPIRATION_DATE);
    HalalCertificationDetailResponse halalCertificationDetailResponse = new HalalCertificationDetailResponse();
    List<HalalCertificationDetailResponse> halalCertificationDetailResponses = new ArrayList<>();
    halalCertificationDetailResponse.setReg_prod_name(PRODUCT_NAME);
    halalCertificationDetailResponse.setSertifikat(halalCeritficationDetails);
    halalCertificationDetailResponses.add(halalCertificationDetailResponse);
    bpjphData.setDatas(halalCertificationDetailResponses);
    List<HalalCertificationWebDetailsResponse> responseList =
        ResponseHelper.toHalalCertificationDetailResponse(bpjphData);
    Assertions.assertEquals(CERTIFICATION_NUMBER, responseList.get(0).getCertificationNumber());
    Assertions.assertEquals(ISSUED_DATE, responseList.get(0).getIssuedDate());
    Assertions.assertEquals(EXPIRATION_DATE, responseList.get(0).getExpirationDate());
  }

  @Test
  public void toHalalCertificationDetailResponseNullCertificateTest() {
    BPJPHData bpjphData = new BPJPHData();
    HalalCeritficationDetails halalCeritficationDetails = new HalalCeritficationDetails();
    halalCeritficationDetails.setNo_sert(CERTIFICATION_NUMBER);
    halalCeritficationDetails.setTgl_sert(ISSUED_DATE);
    halalCeritficationDetails.setTgl_valid(EXPIRATION_DATE);
    HalalCertificationDetailResponse halalCertificationDetailResponse = new HalalCertificationDetailResponse();
    List<HalalCertificationDetailResponse> halalCertificationDetailResponses = new ArrayList<>();
    halalCertificationDetailResponse.setReg_prod_name(PRODUCT_NAME);
    halalCertificationDetailResponse.setSertifikat(null);
    halalCertificationDetailResponses.add(halalCertificationDetailResponse);
    bpjphData.setDatas(halalCertificationDetailResponses);
    List<HalalCertificationWebDetailsResponse> responseList =
        ResponseHelper.toHalalCertificationDetailResponse(bpjphData);
  }

  @Test
  public void getHalaDashboardProductsWebResponseTest() {
    HalalDashboardProductsResponse halalDashboardProductsResponse = new HalalDashboardProductsResponse();
    String halalProductLinkPrefix = "";
    HalalDashboardProductsWebResponse response =
        ResponseHelper.getHalaDashboardProductsWebResponse(halalDashboardProductsResponse, halalProductLinkPrefix);
    Assertions.assertEquals(response.getProductSku(), halalDashboardProductsResponse.getProductSku());
  }


  @Test
  public void checkMismatchforCatgoryTest1(){
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew = Optional.of(new OrderItemMarginsResponse());
    List<Margin> margins = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginPercentage(10.0);
    margins.add(margin);
    orderItemMarginsResponseForNew.get().setMargins(margins);
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting = Optional.of(new OrderItemMarginsResponse());

    OrderItemMarginsResponse orderItemMarginsResponse1 = new OrderItemMarginsResponse();
    List<Margin> margins1 = new ArrayList<>();
    Margin margin1 = new Margin();
    margin1.setMarginPercentage(19.0);
    margins1.add(margin1);
    orderItemMarginsResponseForExisting.get().setMargins(margins1);
    ResponseHelper.checkMismatchForCategory(orderItemMarginsResponseForNew,orderItemMarginsResponseForExisting);
  }

  @Test
  public void checkMismatchforCatgoryTest4(){
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew = Optional.of(new OrderItemMarginsResponse());
    List<Margin> margins = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginPercentage(10.0);
    margins.add(margin);
orderItemMarginsResponseForNew.get().setMargins(margins);
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting = Optional.of(new OrderItemMarginsResponse());

    OrderItemMarginsResponse orderItemMarginsResponse1 = new OrderItemMarginsResponse();
    List<Margin> margins1 = new ArrayList<>();
    Margin margin1 = new Margin();
    margin1.setMarginPercentage(9.0);
    margins1.add(margin1);
    orderItemMarginsResponseForExisting.get().setMargins(margins1);
    ResponseHelper.checkMismatchForCategory(orderItemMarginsResponseForNew,orderItemMarginsResponseForExisting);
  }

  @Test
  public void checkMismatchforCatgoryNullTest(){
    ResponseHelper.checkMismatchForCategory(null,null);
  }

  @Test
  public void checkMismatchforCatgoryNullTest1(){
    ResponseHelper.checkMismatchForCategory(Optional.empty(),null);
  }

  @Test
  public void checkMismatchforCatgoryTest5(){
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew = Optional.of(new OrderItemMarginsResponse());
    List<Margin> margins = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginPercentage(10.0);
    margins.add(margin);
    orderItemMarginsResponseForNew.get().setMargins(margins);
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting = Optional.of(new OrderItemMarginsResponse());

    OrderItemMarginsResponse orderItemMarginsResponse1 = new OrderItemMarginsResponse();
    List<Margin> margins1 = new ArrayList<>();
    Margin margin1 = new Margin();
    margin1.setMarginPercentage(null);
    margins1.add(margin1);
    orderItemMarginsResponseForExisting.get().setMargins(margins1);
    ResponseHelper.checkMismatchForCategory(orderItemMarginsResponseForNew,orderItemMarginsResponseForExisting);
  }

  @Test
  public void checkMismatchforCatgoryTest7(){
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew = Optional.of(new OrderItemMarginsResponse());
    List<Margin> margins = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginPercentage(10.0);
    margins.add(null);
    orderItemMarginsResponseForNew.get().setMargins(margins);
    Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting = Optional.of(new OrderItemMarginsResponse());

    OrderItemMarginsResponse orderItemMarginsResponse1 = new OrderItemMarginsResponse();
    List<Margin> margins1 = new ArrayList<>();
    Margin margin1 = new Margin();
    margin1.setMarginPercentage(19.0);
    margins1.add(margin1);
    orderItemMarginsResponseForExisting.get().setMargins(margins1);
    ResponseHelper.checkMismatchForCategory(orderItemMarginsResponseForNew,orderItemMarginsResponseForExisting);
  }

  @Test
  public void populatePriceToResponseTest() {
    ProductDetailWebResponse productDetailWebResponse = new ProductDetailWebResponse();
    ProductItemWebResponse productItemWebResponse = new ProductItemWebResponse();
    productItemWebResponse.setSkuCode(SKU_CODE_1);
    productDetailWebResponse.setProductItemResponses(Collections.singleton(productItemWebResponse));
    DistributionProductItemResponse distributionProductItemResponse =
      new DistributionProductItemResponse();
    distributionProductItemResponse.setSkuCode(SKU_CODE);
    ResponseHelper.populatePriceToResponse(
      Collections.singletonList(distributionProductItemResponse), productDetailWebResponse);
  }

  @Test
  public void setValueConcatenatedWithValueTypeTest() {
    ProductDetailWebResponse productDetailWebResponse = new ProductDetailWebResponse();
    ProductAttributeWebResponse productAttributeWebResponse = new ProductAttributeWebResponse();
    ProductAttributeValueWebResponse productAttributeValueWebResponse =
        new ProductAttributeValueWebResponse();
    ProductAttributeValueWebResponse productAttributeValueWebResponse2 =
        new ProductAttributeValueWebResponse();
    ProductAttributeValueWebResponse productAttributeValueWebResponse3 =
        new ProductAttributeValueWebResponse();
    AllowedAttributeValueWebResponse allowedAttributeValueWebResponse =
        new AllowedAttributeValueWebResponse();
    allowedAttributeValueWebResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    allowedAttributeValueWebResponse.setValueType(ALLOWED_ATTRIBUTE_VALUE_TYPE);
    productAttributeValueWebResponse.setAllowedAttributeValue(allowedAttributeValueWebResponse);

    AllowedAttributeValueWebResponse allowedAttributeValueWebResponse2 =
        new AllowedAttributeValueWebResponse();
    allowedAttributeValueWebResponse.setValue(ALLOWED_ATTRIBUTE_VALUE);
    productAttributeValueWebResponse2.setAllowedAttributeValue(allowedAttributeValueWebResponse2);
    productAttributeWebResponse.setProductAttributeValues(
        Arrays.asList(productAttributeValueWebResponse, productAttributeValueWebResponse2,
            productAttributeValueWebResponse3));
    productDetailWebResponse.setProductAttributeResponses(
        Arrays.asList(productAttributeWebResponse));
    ProductDetailWebResponse response =
        ResponseHelper.setValueConcatenatedWithValueType(productDetailWebResponse,
            StringUtils.EMPTY);
    Assertions.assertEquals(
        ALLOWED_ATTRIBUTE_VALUE_TYPE.concat(StringUtils.EMPTY).concat(ALLOWED_ATTRIBUTE_VALUE),
        response.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
            .getAllowedAttributeValue().getValue());
  }

  @Test
  public void setValueConcatenatedWithValueTypeWithNullWebResponse() {
    ProductDetailWebResponse response = ResponseHelper.setValueConcatenatedWithValueType(null, " ");
    Assertions.assertNull(response);
  }

  @Test
  void testPopulateAiGeneratedFields() {
    AiGeneratedFieldsResponse aiFields = new AiGeneratedFieldsResponse();
    aiFields.setAiGeneratedBrand(BRAND_FLAG);
    aiFields.setAiGeneratedCategory(CATEGORY_FLAG);

    ProductDetailCompleteResponse completeResponse = new ProductDetailCompleteResponse();
    completeResponse.setAiGeneratedFieldsResponse(aiFields);

    GdnRestSingleResponse<ProductDetailCompleteResponse> response = new GdnRestSingleResponse<>();
    response.setValue(completeResponse);

    ProductDetailWebResponse webResponse = new ProductDetailWebResponse();

    ResponseHelper.populateAiGeneratedFields(webResponse, response);

    Assertions.assertEquals(BRAND_FLAG, webResponse.isAiGeneratedBrand());
    Assertions.assertEquals(CATEGORY_FLAG, webResponse.isAiGeneratedCategory());
  }

  @Test
  void testPopulateAiGeneratedFields_whenResponseOrAiFieldsNull() {
    ProductDetailWebResponse webResponse = new ProductDetailWebResponse();
    GdnRestSingleResponse<ProductDetailCompleteResponse> response = new GdnRestSingleResponse<>();

    ResponseHelper.populateAiGeneratedFields(webResponse, null);
    ResponseHelper.populateAiGeneratedFields(webResponse, response);
    Assertions.assertFalse(webResponse.isAiGeneratedBrand());
    Assertions.assertFalse(webResponse.isAiGeneratedCategory());
  }

}
