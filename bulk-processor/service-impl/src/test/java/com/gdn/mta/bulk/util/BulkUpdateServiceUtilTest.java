package com.gdn.mta.bulk.util;


import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.B2BResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.dto.inventory.WarehouseInventoryResponseDTO;
import com.gdn.mta.bulk.dto.inventory.WebInventoryResponseDTO;
import com.gdn.mta.bulk.service.PCBOutboundServiceBean;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkCncUpsertErrorDTO;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.VatUpdateDto;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.feignConfig.XBPFeign;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.repository.pcb.ProductCategoryBaseRepository;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

/**
 * Created by priyanka on 24/02/17.
 */
public class BulkUpdateServiceUtilTest {
  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "x-bulk";
  private static final String REQUEST_ID = "request123";
  private static final String CLIENT_ID = "client123";
  private static final String USERNAME = "username";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "1";
  private static final String PRODUCT_LEVEL_3_UPDATE_PRIORITY_1 = "ProductLevel3UpdatePriority1";
  private static final String INTERNAL_USER_BULK_PROCESS_TYPE = "InternalUserBulkProcess";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "TEC-67823";
  private static final String FILE_NAME = "file1";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DESCRIPTION = "some description";
  private static final String CAMPAIGN_CODE = "CAMP-XXXXX";
  private static final Map<String, Boolean> PRIVILEGED_MAP =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", true)
          .put("isPrivilegedToEditPrice", true).put("isPrivilegedToEditProductType", true)
          .put("isPrivilegedToEditDisplayBuyable", true).put("isPrivilegedToEditPickupPoint", true)
          .put("isPrivilegedToEditO2O", true).build();
  private static final int PRIVILIGED_MAP_SIZE = 6;
  private static final String DEFAULT_BLIBLI = "TEC-15624";
  private static final String DEFAULT_PRODUCT_NAME = "something";
  private static final String BULK_PROCESS_CODE = "9092";
  private static final String DEFAULT_STATUS = "PENDING";
  private static final String ERROR_MSG = "error";
  private static final String PICKUP_POINT_CODE = "PP-12345";
  private static final String PICKUP_POINT_NAME = "PP-12345";
  private static final String PICKUP_POINT_CODE_1 = "PP-123456";
  private static final String PICKUP_POINT_NAME_1 = "PP-123456";
  private static final String ITEM_SKU = "TEC-15624-00001-00001";
  private static final String BUSINESS_PARTNER_CODE = "TEC-15624";
  private static final String PRODUCT_SKU = "TEC-15624-00001";
  private static final String BRAND_NAME = "Brand";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BP_CODE = "businessPartnerCode";

  private static final String CONTENT_ASSIGN = "content";
  private static final String UPDATED_BY = "updatedBy";
  private static final String ACTION_TYPE_MERCHANT = "ACTION_TYPE_MERCHANT";
  private static final String FILE_PATH = "filePath";
  private static final int MINIMUM_PRICE = 1;
  private static final String SELLER_SKU_1 = "31";
  private static final String SELLER_SKU_2 = "31.0";
  private static final String SELLER_SKU_3 = "ABC67521";
  private static final String SELLER_SKU_4 = "000000067521";
  private static final String BULK_VAT_DIRECTORY_1 = "BulkVat/test1.xlsx";
  private static final String BULK_VAT_DIRECTORY_2 = "BulkVat/test2.xlsx";
  private static final String BULK_VAT_DIRECTORY_3 = "BulkVat/test3.xlsx";
  private static final String BULK_VAT_DIRECTORY_4 = "BulkVat/test4.xlsx";
  private static final String PARENT_1 = "parent1";
  private static final String PARENT_2 = "parent2";
  private static final String FAAS_ACTIVATED = "faasActivated";
  private String FBB_ITEM_SKU_1 = "FBB_ITEM_SKU_1";
  private String FBB_ITEM_SKU_2 = "FBB_ITEM_SKU_2";
  private String FBB_ITEM_SKU_3 = "FBB_ITEM_SKU_3";
  private String FBB_ITEM_PP_CODE_1 = "FBB_ITEM_PP_CODE_1";
  private String FBB_ITEM_PP_CODE_2 = "FBB_ITEM_PP_CODE_2";

  private static final Map<String, Boolean> PRIVILEGE_MAP =
      ImmutableMap.<String, Boolean>builder()
          .put(BulkParameters.PRIVILEGE_EDIT_PRICE, true)
          .put(BulkParameters.PRIVILEGE_EDIT_STOCK, true)
          .put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true)
          .put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true)
          .put(BulkParameters.PRIVILEGE_EDIT_O2O, true)
          .put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true)
          .put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true)
          .build();

  private BulkVendorProductAssignRequest bulkVendorProductAssignRequest;
  private BulkProductSuspensionRequest bulkProductSuspensionRequest;
  private List<Map<String, String>> successDatas;
  private List<Map<String, String>> failureDatas;
  private List<Map<String,String>> cleanDatas;
  private List<String> pickupPointCodes;
  private MandatoryRequestParam mandatoryRequestParam;
  private BulkProcessUpdateRequest bulkProcessUpdateRequest;
  private BulkUpdateProcessDTO bulkUpdateProcessDTO;
  private BulkAddCampaignProductDTO bulkAddCampaignProductDTO;
  private BulkUpdateErrorCounter counter;
  private BulkProcess bulkProcess;
  private BulkProcessNotes bulkProcessNotes;
  private List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO;
  private List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList;
  private BulkAddCampaignProductQueue bulkAddCampaignProductQueue;
  private MasterDataBulkUpdateRequest masterDataBulkUpdateRequest;
  private BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest;
  private ItemSummaryListResponse itemSummaryListResponse;
  private ItemSummaryListResponse itemSummaryListResponse1;
  private ItemPickupPointRequest itemPickupPointRequest;
  private ItemDetailsDto itemDetailsDto;
  private PriceDTO priceDto;
  private ObjectMapper mapper = new ObjectMapper();
  private ProfileResponse profileResponse;
  private BasicProductResponse basicProductResponse;
  private PreOrderDTO preOrderDTO;
  Map<Integer, String> headers;

  private static final Map<String, String> headerMap =
      ImmutableMap.<String, String>builder()
          .put(BulkParameters.PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.SELLING_PRICE_HEADER, BulkParameters.PRIVILEGE_EDIT_PRICE)
          .put(BulkParameters.STOCK_HEADER, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.STOCK_REMINDER_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_STOCK)
          .put(BulkParameters.TYPE_HANDLING_HEADER, BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE)
          .put(BulkParameters.PICKUP_POINT_HEADER, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.PICKUP_POINT_NAME_COLUMN_ID, BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT)
          .put(BulkParameters.OFFLINE_TO_ONLINE_HEADER, BulkParameters.PRIVILEGE_EDIT_O2O)
          .put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P,
              BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .put(BulkParameters.CNC_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_CNC_STATUS)
          .put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE)
          .build();

  @InjectMocks
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductCategoryBaseRepository productCategoryBaseRepository;

  @Mock
  private CampaignRepository campaignRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private XBPFeign xbpFeign;

  @Mock
  private PCBOutboundServiceBean pcbOutboundServiceBean;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    successDatas = new ArrayList<>();
    failureDatas = new ArrayList<>();
    pickupPointCodes = getPickupPointCodes();
    counter = new BulkUpdateErrorCounter();
    bulkProcessNotes = new BulkProcessNotes();
    List<BulkProcessNotes> bulkProcessNoteList = new ArrayList<>();
    bulkProcessNoteList.add(bulkProcessNotes);

    listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Product 1", "MT-0121", "harga error");
    BulkUpdateErrorDTO bulkUpdateErrorDTO1 =
        new BulkUpdateErrorDTO("Product 1", "MT-0122", "harga  penjualan error");
    BulkUpdateErrorDTO bulkUpdateErrorDTO2 =
        new BulkUpdateErrorDTO("Product 1", "MT-0123", "produck nama error");

    listBulkUpdateErrorDTO.add(bulkUpdateErrorDTO);
    listBulkUpdateErrorDTO.add(bulkUpdateErrorDTO1);
    listBulkUpdateErrorDTO.add(bulkUpdateErrorDTO2);

    bulkCncUpsertErrorDTOList = new ArrayList<>();
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO =
        new BulkCncUpsertErrorDTO("TOQ-60028-00001-00001", "PP-0123456", "error message");
    bulkCncUpsertErrorDTOList.add(bulkCncUpsertErrorDTO);

    bulkProcess =
        new BulkProcess(BULK_PROCESS_CODE, DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new Date(), new Date(), DEFAULT_STATUS, DESCRIPTION, bulkProcessNoteList);
    bulkUpdateProcessDTO =
        new BulkUpdateProcessDTO(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, "system", "mta");
    bulkAddCampaignProductDTO = new BulkAddCampaignProductDTO();
    bulkAddCampaignProductDTO.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkAddCampaignProductDTO.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkAddCampaignProductDTO.setFileContent(new byte[256]);
    bulkAddCampaignProductDTO.setFileName(FILE_NAME);
    bulkAddCampaignProductDTO.setPrivilegedMap(PRIVILEGED_MAP);
    bulkAddCampaignProductDTO.setUpdatedBy("system");
    bulkAddCampaignProductDTO.setClientHost("mta");
    bulkAddCampaignProductDTO.setMaxDiscount(Double.valueOf(1000));
    bulkAddCampaignProductDTO.setMinDiscount(Double.valueOf(100));
    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, REQUEST_ID, CLIENT_ID, USERNAME, null);
    bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, CLIENT_ID);
    bulkAddCampaignProductQueue = new BulkAddCampaignProductQueue();
    bulkAddCampaignProductQueue.setCampaignCode(CAMPAIGN_CODE);
    bulkAddCampaignProductQueue.setStoreId(STORE_ID);
    bulkAddCampaignProductQueue.setRequestId(REQUEST_ID);
    bulkAddCampaignProductQueue.setUpdatedBy(USERNAME);
    bulkAddCampaignProductQueue.setMaxDiscount(Double.valueOf(100000));
    bulkAddCampaignProductQueue.setMinDiscount(Double.valueOf(500));
    bulkAddCampaignProductQueue.setMinQuota(5);
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    List<String> categories = new ArrayList<>();
    categories.add("categoryCode");
    campaignItemSummaryRequest.setCategories(categories);
    campaignItemSummaryRequest.setMerchantCode("merchantCode");
    List<String> brands = new ArrayList<>();
    brands.add("Brand");
    campaignItemSummaryRequest.setBrands(brands);
    bulkAddCampaignProductQueue.setCampaignItemSummaryRequest(campaignItemSummaryRequest);
    masterDataBulkUpdateRequest = new MasterDataBulkUpdateRequest();
    masterDataBulkUpdateRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    masterDataBulkUpdateRequest.setStoreId(STORE_ID);
    masterDataBulkUpdateRequest.setRequestId(REQUEST_ID);

    bulkVendorProductAssignRequest = new BulkVendorProductAssignRequest();
    bulkVendorProductAssignRequest.setAssignmentType(CONTENT_ASSIGN);
    bulkVendorProductAssignRequest.setStoreId(STORE_ID);
    bulkVendorProductAssignRequest.setUpdatedBy(UPDATED_BY);
    bulkVendorProductAssignRequest.setBulkProcessType(INTERNAL_USER_BULK_PROCESS_TYPE);
    bulkVendorProductAssignRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkVendorProductAssignRequest.setRequestId(REQUEST_ID);

    bulkConfigurationUpdateRequest = new BulkConfigurationUpdateRequest();
    bulkConfigurationUpdateRequest.setRequestId(REQUEST_ID);
    bulkConfigurationUpdateRequest.setStoreId(STORE_ID);
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_MERCHANT);
    bulkConfigurationUpdateRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkConfigurationUpdateRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkConfigurationUpdateRequest.setFilePath(FILE_PATH);
    bulkConfigurationUpdateRequest.setUpdatedBy(UPDATED_BY);

    bulkProductSuspensionRequest =
        BulkProductSuspensionRequest.builder().actionType("REACTIVATE").storeId(STORE_ID).build();

    priceDto = new PriceDTO();
    priceDto.setOfferPrice(1000d);
    Set<PriceDTO> priceDTOSet = new HashSet<>();
    priceDTOSet.add(priceDto);
    itemSummaryListResponse =
        ItemSummaryListResponse.builder().itemSku(DEFAULT_BLIBLI).pickupPointCode(PICKUP_POINT_CODE)
            .pickupPointName(PICKUP_POINT_NAME).price(priceDTOSet).masterCategoryCode(CATEGORY_CODE)
            .merchantCode(MERCHANT_CODE).brandName(BRAND_NAME).build();

    itemSummaryListResponse1 =
        ItemSummaryListResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .pickupPointName(PICKUP_POINT_NAME_1).price(priceDTOSet)
            .masterCategoryCode(CATEGORY_CODE).merchantCode(MERCHANT_CODE).brandName(BRAND_NAME)
            .build();


    itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(DEFAULT_BLIBLI);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);

    itemDetailsDto = new ItemDetailsDto();
    itemDetailsDto.setItemSku(DEFAULT_BLIBLI);
    itemDetailsDto.setPickUpPointCode(PICKUP_POINT_CODE);

    profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    headers = new HashMap<>();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel3Repository,
        this.productCategoryBaseRepository, this.campaignRepository, this.bulkProcessService);
  }

  private List<Map<String,String>> generateExcelData(int size){
    List<Map<String,String>> excelData = new ArrayList<>();
    for(int i =0 ; i < size; i++){
      Map<String,String> productRow = new HashMap<>();
      productRow.put(BulkParameters.BLIBLI_SKU, DEFAULT_BLIBLI);
      productRow.put(BulkParameters.PRODUCT_NAME, DEFAULT_PRODUCT_NAME + i);
      productRow.put(BulkParameters.SKU_CODE_HEADER, "001" + 1);
      productRow.put(BulkParameters.SELLER_SKU, "9092");
      productRow.put(BulkParameters.PRICE_HEADER, "3000");
      productRow.put(BulkParameters.SELLING_PRICE_HEADER, "2500");
      productRow.put(BulkParameters.STOCK_HEADER, "100");
      productRow.put(BulkParameters.TYPE_HANDLING_HEADER, "1");
      productRow.put(BulkParameters.PICKUP_POINT_HEADER, "PP-3000297");
      productRow.put(BulkParameters.AMPHI_SKU_STATUS, "0");
      productRow.put(BulkParameters.EXTERNAL_SKU_STATUS, "1.0");
      excelData.add(productRow);
    }
    return excelData;
  }

  private List<Map<String,String>> generateExcelProductCampaignData(int size) {
    List<Map<String, String>> excelData = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      Map<String, String> productRow = new HashMap<>();
      productRow.put(BulkParameters.BLIBLI_SKU, DEFAULT_BLIBLI);
      productRow.put(BulkParameters.POTONGAN_HARGA, "5000");
      productRow.put(BulkParameters.KUOTA, "1");
      excelData.add(productRow);
    }
    return excelData;
  }

  @Test
  public void mandatoryRequestParamValidationSuccess() throws Exception {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
  }

  @Test
  public void mandatoryRequestParamValidationStoreIdBlank() throws Exception {
    mandatoryRequestParam.setStoreId(StringUtils.EMPTY);
    try {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(
          ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.STORE_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void mandatoryRequestParamValidationChannelIdBlank() throws Exception {
    mandatoryRequestParam.setChannelId(StringUtils.EMPTY);
    try {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(
          ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.CHANNEL_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void mandatoryRequestParamValidationRequestIdBlank() throws Exception {
    mandatoryRequestParam.setRequestId(StringUtils.EMPTY);
    try {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(
          ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.REQUEST_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void mandatoryRequestParamValidationClientIdBlank() throws Exception {
    mandatoryRequestParam.setClientId(StringUtils.EMPTY);
    try {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(
          ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.CLIENT_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void mandatoryRequestParamValidationUsernameBlank() throws Exception {
    mandatoryRequestParam.setUsername(StringUtils.EMPTY);
    try {
      BulkUpdateServiceUtil.mandatoryRequestParamValidation(mandatoryRequestParam);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(
          ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.USERNAME_MUST_NOT_BE_BLANK,
          e.getMessage());
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void validateExcelDatasBulkUpdateProductCleanDataNull_Success() throws Exception{
    cleanDatas = null;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =  bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(0,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0,failureDatas.size());
    Assertions.assertEquals(0,successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductCleanDatASuccess() throws Exception{
    cleanDatas = null;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =  bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, StringUtils.EMPTY);
    Assertions.assertEquals(0,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0,failureDatas.size());
    Assertions.assertEquals(0,successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanDataNull_Success() throws Exception{
    cleanDatas = null;
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =  bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
            failureDatas, counter, new BulkAddCampaignProductQueue());
    Assertions.assertEquals(0,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0,failureDatas.size());
    Assertions.assertEquals(0,successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct_Success() throws Exception{
    cleanDatas = generateExcelData(1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(0,failureDatas.size());
    Assertions.assertEquals(1,successDatas.size());
    Assertions.assertEquals(0,counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct_WithWrongHeader() throws Exception{
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).remove(BulkParameters.BLIBLI_SKU);
    cleanDatas.get(0).remove(BulkParameters.PRODUCT_NAME);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct_WithEmptyValue() throws Exception{
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY);
    cleanDatas.get(0).put(BulkParameters.PRODUCT_NAME, StringUtils.EMPTY);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    Assertions.assertEquals(1, counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct() throws Exception {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, PRODUCT_SKU);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_PRODUCT_SKU, ITEM_SKU);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct2() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, PRODUCT_SKU);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_PRODUCT_SKU, ITEM_SKU);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, StringUtils.EMPTY);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductEmpty() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, StringUtils.EMPTY);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_PRODUCT_SKU, StringUtils.EMPTY);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct3() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).remove(BulkParameters.BLIBLI_SKU);
    cleanDatas.get(0).remove(BulkParameters.BLIBLI_PRODUCT_SKU);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct4() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, PARENT_1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_PRODUCT_SKU, PARENT_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
  }

  @ParameterizedTest
  @ValueSource(strings = {"validQuota","-","Negative","NumberFormatExp","CounterGt100","NegativeCounterGt100"})
  public void validateExcelDatasForBulkUpdateWithPoQuota(String test) {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, PARENT_1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_PRODUCT_SKU, PARENT_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    if (test.equals("validQuota")) {
      cleanDatas.get(0).put("PO Quota", "123");
    } else if (test.equals("-")) {
      cleanDatas.get(0).put("PO Quota", "-");
    } else if (test.equals("Negative")) {
      cleanDatas.get(0).put("PO Quota", "-123");
    } else if (test.equals("NumberFormatExp")) {
      cleanDatas.get(0).put("PO Quota", "abc");
    } else if (test.equals("CounterGt100")) {
      cleanDatas.get(0).put("PO Quota", "abc");
      for (int i = 0; i < 101; i++) {
        counter.incrementPoQuotaCounter();
      }
    } else if(test.equals("NegativeCounterGt100")){
      for (int i = 0; i < 101; i++) {
        counter.incrementPoQuotaCounter();
      }
      cleanDatas.get(0).put("PO Quota", "-123");
    }
    bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, true, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_SuccessMPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_SuccessMPP_EmptyPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_SuccessPricingMPP_EmptyPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);
    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
        .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
            bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(xProductOutboundService)
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    verify(this.campaignRepository)
        .getCampaignProductDetailsV2(anyList(), Mockito.eq(bulkAddCampaignProductQueue));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_FailedPricingMPP_EmptyPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
        .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
            bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE_1);
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
      anyBoolean());
    verify(this.campaignRepository).getCampaignProductDetailsV2(anyList(),
        Mockito.eq(bulkAddCampaignProductQueue));
    verify(xProductOutboundService)
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_SuccessMPP_ValidPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
  }

  private void getProductLevel3Responses(List<ProductLevel3SummaryResponse> responses) {
    ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
    response.setMerchantCode("merchantCode");
    response.setCategoryCode("categoryCode");
    response.setBrand("Brand");
    response.setItemSku(DEFAULT_BLIBLI);
    response.setPickupPointCode(PICKUP_POINT_CODE);
    response.setPrices(Lists.newArrayList(new ProductLevel3PriceResponse(null, 10000.0, 10000.0)));
    responses.add(response);
  }

  private List<CampaignProductDetailResponse> getCampaignProductDetailResponses() {
    List<CampaignProductDetailResponse> campaignProductDetailResponses = new ArrayList<>();
    CampaignProductDetailResponse campaignProductDetailResponse =
        new CampaignProductDetailResponse();
    campaignProductDetailResponse.setItemSku(DEFAULT_BLIBLI);
    campaignProductDetailResponse.setUsedQuota(0);
    campaignProductDetailResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    campaignProductDetailResponses.add(campaignProductDetailResponse);
    return campaignProductDetailResponses;
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithDuplicateBlibliSKU_Success()
      throws Exception {
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    getProductLevel3Responses(responses);
    cleanDatas = generateExcelProductCampaignData(2);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
  }

  @Test
  public void updateBulkProcessNotesForProductCampaignTest() throws Exception {
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    listBulkUpdateErrorDTO.add(bulkUpdateErrorDTO);
    BulkProcess bulkProcess = new BulkProcess();
    bulkUpdateServiceUtil.updateBulkProcessNotesForCampaignOrCncProduct(listBulkUpdateErrorDTO, STORE_ID, BULK_PROCESS_CODE, bulkProcess);
  }

  @Test
  public void updateBulkProcessNotesForProductCampaignTestAlreadyExist() throws Exception {
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorDTO bulkUpdateErrorDTO = new BulkUpdateErrorDTO();
    listBulkUpdateErrorDTO.add(bulkUpdateErrorDTO);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessNotes(new ArrayList<>());
    bulkUpdateServiceUtil.updateBulkProcessNotesForCampaignOrCncProduct(listBulkUpdateErrorDTO, STORE_ID, BULK_PROCESS_CODE, bulkProcess);
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_ExceptionFromPbp()
      throws Exception {
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();

    ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
    response.setItemSku(DEFAULT_BLIBLI);
    responses.add(response);
    cleanDatas = generateExcelProductCampaignData(1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
      anyBoolean());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithEmptyBlibliSKUAndProductName() throws Exception{
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU,StringUtils.EMPTY);
    cleanDatas.get(0).put(BulkParameters.PRODUCT_NAME,StringUtils.EMPTY);
    List<String> pickupPointCodes = getPickupPointCodes();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(0,successDatas.size());
    Assertions.assertEquals(1,counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithStockInvalid() throws Exception{
    cleanDatas = generateExcelData(3);
    cleanDatas.get(1).put(BulkParameters.STOCK_HEADER,"-1");
    cleanDatas.get(2).put(BulkParameters.STOCK_HEADER,"abc");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(2,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(2,failureDatas.size());
    Assertions.assertEquals(1,successDatas.size());
    Assertions.assertEquals(2,counter.getStockCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithStockSkip() throws Exception {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.STOCK_HEADER, "-");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes,
            successDatas, failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(0, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithoutStock() throws Exception {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).remove(BulkParameters.STOCK_HEADER);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes,
            successDatas, failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(0, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithHargaInvalid() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(1).put(BulkParameters.PRICE_HEADER,"-1000");
    cleanDatas.get(2).put(BulkParameters.PRICE_HEADER,"0");
    cleanDatas.get(3).put(BulkParameters.PRICE_HEADER,"abc");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(3,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(3,failureDatas.size());
    Assertions.assertEquals(2,successDatas.size());
    Assertions.assertEquals(5,counter.getHargaCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithSalePriceInvalid() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(1).put(BulkParameters.SELLING_PRICE_HEADER, "-1000");
    cleanDatas.get(2).put(BulkParameters.SELLING_PRICE_HEADER, "abc");
    cleanDatas.get(3).put(BulkParameters.SELLING_PRICE_HEADER, "0");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(3,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(3,failureDatas.size());
    Assertions.assertEquals(2,successDatas.size());
    Assertions.assertEquals(3,counter.getHargaPenjualanCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithPriceLessThanSalePrice() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PRICE_HEADER, "3000");
    cleanDatas.get(0).put(BulkParameters.SELLING_PRICE_HEADER, "3500");
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, StringUtils.EMPTY);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(4,successDatas.size());
    Assertions.assertEquals(1,counter.getHargaCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithNoSellerSku() throws Exception{
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).remove(BulkParameters.SELLER_SKU);
    cleanDatas.get(0).put(BulkParameters.PRICE_HEADER, "3000");
    cleanDatas.get(0).put(BulkParameters.SELLING_PRICE_HEADER, "3500");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(0,successDatas.size());
    Assertions.assertEquals(1,counter.getHargaCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithInvalidSellerSku() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PRICE_HEADER, "3000");
    cleanDatas.get(0).put(BulkParameters.SELLING_PRICE_HEADER, "3500");
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, new String(new char[300]).replace('\0', ' '));
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(4,successDatas.size());
    Assertions.assertEquals(1,counter.getHargaCounter());
    Assertions.assertEquals(1, counter.getSellerSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithInvalidSellerSkuErrorCounter() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PRICE_HEADER, "3000");
    cleanDatas.get(0).put(BulkParameters.SELLING_PRICE_HEADER, "3500");
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, new String(new char[300]).replace('\0', ' '));
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    counter.setSellerSkuCounter(101);
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(4,successDatas.size());
    Assertions.assertEquals(1,counter.getHargaCounter());
    Assertions.assertEquals(102, counter.getSellerSkuCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithPickupPointCodesInvalid() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_HEADER, "abc");
    cleanDatas.get(3).put(BulkParameters.PICKUP_POINT_HEADER, StringUtils.EMPTY);
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, BulkParameters.SELLER_SKU);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(2,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(2,failureDatas.size());
    Assertions.assertEquals(3,successDatas.size());
    Assertions.assertEquals(2,counter.getPickupPointCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithPickupPointCodesInaccessible() throws Exception {
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_HEADER, "PP-3000298");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, Collections.singleton("PP-3000297"), false, StringUtils.EMPTY);
    Assertions.assertEquals(1, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(4, successDatas.size());
    Assertions.assertEquals(1, counter.getPickupPointCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithPickupPointCodesInaccessible_moreThanCounter() throws Exception {
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_HEADER, "PP-3000298");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    counter.setPickupPointCounter(100);
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, Collections.singleton("PP-3000297"), false, StringUtils.EMPTY);
    Assertions.assertEquals(1, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(4, successDatas.size());
    Assertions.assertEquals(101, counter.getPickupPointCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProduct_noPickupPointHeader() throws Exception {
    cleanDatas = generateExcelData(5);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    cleanDatas.forEach(cleanData -> cleanData.remove(BulkParameters.PICKUP_POINT_HEADER));
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, Collections.singleton("PP-3000297"), false, StringUtils.EMPTY);
    Assertions.assertEquals(0, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(5, successDatas.size());
    Assertions.assertEquals(0, counter.getPickupPointCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithPickupPointCodesAccessible() throws Exception {
    cleanDatas = generateExcelData(5);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO =
        bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, Collections.singleton("PP-3000297"), false, StringUtils.EMPTY);
    Assertions.assertEquals(0, bulkUpdateErrorDTO.size());
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(5, successDatas.size());
    Assertions.assertEquals(0, counter.getPickupPointCounter());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithBooleanHeadersInvalid() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(3).put(BulkParameters.PURCHASED_HEADER, "abc");
    cleanDatas.get(3).put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
        .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
            failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(1,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(1,failureDatas.size());
    Assertions.assertEquals(4,successDatas.size());
    Assertions.assertEquals(1,counter.getBooleanHeaderCounter());
  }

  @Test
  public void getBulkProcessTest() throws Exception {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "false",
            SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED));
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateProcessDTO, 0, 0, false, false);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkProcess.getBusinessPartnerCode());
    Assertions.assertEquals(STORE_ID,bulkProcess.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkProcess.getBulkProcessType());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessTestwithUpdatePriorityQueueEnabled() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BP_CODE);
    profileResponse.setTrustedSeller(true);
    bulkUpdateProcessDTO.setBulkProcessType(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    when(businessPartnerRepository
        .filterByBusinessPartnerCodeV2(Constant.STORE_ID, bulkProcess.getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateProcessDTO, 0, 0, true, false);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkProcess.getBusinessPartnerCode());
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1, bulkProcess.getBulkProcessType());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessTestwithUpdatePriorityQueueEnabledAndNullProfileResponse() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    bulkUpdateProcessDTO.setBulkProcessType(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateProcessDTO, 0, 0, false, true);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkProcess.getBusinessPartnerCode());
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1, bulkProcess.getBulkProcessType());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessTestwithUpdatePriorityQueueEnabledAndNullProfileResponseAndTrustedSeller()
      throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(true);
    bulkUpdateProcessDTO.setBulkProcessType(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    when(businessPartnerRepository.filterByBusinessPartnerCodeV2(anyString(), anyString())).thenReturn(profileResponse);
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateProcessDTO, 0, 0, true, false);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkProcess.getBusinessPartnerCode());
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1, bulkProcess.getBulkProcessType());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessTestwithUpdatePriorityQueueEnabledAndNullProfileResponseAndNotTrustedSeller()
      throws Exception {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse = new GdnRestSingleResponse<ProfileResponse>();
    gdnRestSingleResponse.setValue(null);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED)).thenReturn(
        new SystemParameterConfig(SystemParameterConfigNames.UPDATE_PRIORITY_QUEUE_ENABLED, "true",
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED));
    Mockito.when(xbpFeign
        .filterByBusinessPartnerCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(gdnRestSingleResponse);
    BulkProcess bulkProcess = bulkUpdateServiceUtil
        .getBulkProcess(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateProcessDTO, 0, 0, true, true);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkProcess.getBusinessPartnerCode());
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(PRODUCT_LEVEL_3_UPDATE_PRIORITY_1, bulkProcess.getBulkProcessType());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessTestWithTwoArguments() {
    BulkProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(masterDataBulkUpdateRequest, 0, 0);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(INTERNAL_USER_BULK_PROCESS_TYPE, bulkProcess.getBulkProcessType());
    Assertions.assertEquals(DEFAULT_STATUS, bulkProcess.getStatus());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcessVendorAssignmentTest() {
    BulkProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(INTERNAL_USER_BULK_PROCESS_TYPE, bulkProcess.getBulkProcessType());
    Assertions.assertEquals(DEFAULT_STATUS, bulkProcess.getStatus());
    Assertions.assertEquals(UPDATED_BY, bulkProcess.getCreatedBy());
    Assertions.assertTrue(bulkProcess.getBulkUpdate());
  }

  @Test
  public void getBulkProcess() {
    BulkInternalProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcess.getSuccessCount());
  }

  @Test
  public void getBulkUpdateQueueTest(){
    BulkUpdateQueue bulkUpdateQueue = bulkUpdateServiceUtil
        .getBulkUpdateQueue(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            bulkUpdateProcessDTO);
    Assertions.assertEquals(bulkUpdateProcessDTO.getBusinessPartnerCode(),
        bulkUpdateQueue.getBusinessPartnerCode());
    Assertions.assertEquals(bulkUpdateProcessDTO.getFileName(), bulkUpdateQueue.getFileName());
    Assertions.assertEquals(PRIVILIGED_MAP_SIZE, bulkUpdateQueue.getPrivilegedMap().size());
    Assertions.assertEquals(STORE_ID, bulkUpdateQueue.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkUpdateQueue.getRequestId());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateQueue.getBusinessPartnerCode());
  }

  @Test
  public void getBulkAddCampaignProductQueueTest() throws Exception {
    BulkUpdateQueue bulkUpdateQueue = bulkUpdateServiceUtil
        .getBulkAddCampaignProductQueue(STORE_ID, REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            bulkAddCampaignProductDTO);
    Assertions.assertEquals(bulkUpdateProcessDTO.getBusinessPartnerCode(),
        bulkUpdateQueue.getBusinessPartnerCode());
    Assertions.assertEquals(bulkUpdateProcessDTO.getFileName(), bulkUpdateQueue.getFileName());
    Assertions.assertEquals(PRIVILIGED_MAP_SIZE, bulkUpdateQueue.getPrivilegedMap().size());
    Assertions.assertEquals(STORE_ID, bulkUpdateQueue.getStoreId());
    Assertions.assertEquals(REQUEST_ID, bulkUpdateQueue.getRequestId());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, bulkUpdateQueue.getBusinessPartnerCode());
  }


  @Test
  public void updateBulkErrorsCampaingErrorTest(){
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    cleanDatas = generateExcelData(3);
    cleanDatas.get(0).put(BulkParameters.TYPE_HANDLING_HEADER, "10");
    cleanDatas.get(0).put(BulkParameters.ERROR_CODE, ERROR_MSG);
    cleanDatas.get(2).put(BulkParameters.PRODUCT_NAME, StringUtils.EMPTY);
    List<Map<String,String>> validPassData = generateExcelData(1);
    List<Map<String,String>> failureDatas = cleanDatas;
    bulkUpdateServiceUtil.updateBulkErrors(failureDatas, listBulkUpdateErrorDTO, counter,
      validPassData, new HashSet<>(), true);
    Assertions.assertEquals(3, counter.getSystemErrorCounter());
    Assertions.assertEquals(3, listBulkUpdateErrorDTO.size());
  }

  @Test
  public void updateBulkErrorsCampaignFalseTest(){
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    cleanDatas = generateExcelData(3);
    cleanDatas.get(0).put(BulkParameters.TYPE_HANDLING_HEADER, "10");
    cleanDatas.get(0).put(BulkParameters.ERROR_CODE, ERROR_MSG);
    cleanDatas.get(2).put(BulkParameters.PRODUCT_NAME, StringUtils.EMPTY);
    List<Map<String,String>> validPassData = generateExcelData(1);
    List<Map<String,String>> failureDatas = cleanDatas;
    bulkUpdateServiceUtil.updateBulkErrors(failureDatas, listBulkUpdateErrorDTO, counter,
      validPassData, new HashSet<>(), false);
    Assertions.assertEquals(3, counter.getSystemErrorCounter());
    Assertions.assertEquals(3, listBulkUpdateErrorDTO.size());
  }

  @Test
  public void updateBulkErrorsTest_No_Error_COde(){
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    cleanDatas = generateExcelData(1);
    failureDatas = cleanDatas;
    List<Map<String,String>> validPassData = cleanDatas;

    bulkUpdateServiceUtil.updateBulkErrors(failureDatas, listBulkUpdateErrorDTO, counter,
        validPassData, new HashSet<>(), false);
    Assertions.assertEquals(1, counter.getSystemErrorCounter());
    Assertions.assertEquals(1, listBulkUpdateErrorDTO.size());
  }

  @Test
  public void testAddErrorCounterAndConstructErrorMessageForCncBulkDelete() {
    List<BulkCncUpsertErrorDTO> bulkCncUpsertErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    cleanDatas = generateInstantPickupProductExcelData(1);
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, "PP-0123456");
    List<Map<String, String>> failureDatas = cleanDatas;
    bulkUpdateServiceUtil.addErrorCounterAndConstructErrorMessageForCncBulkDelete(failureDatas,
        bulkCncUpsertErrorDTOList, counter);
    Assertions.assertEquals(1, counter.getSystemErrorCounter());
    Assertions.assertEquals(1, bulkCncUpsertErrorDTOList.size());
  }

  @Test
  public void updateBulkStatusAbortedTest(){
    counter.setInputErrorCounter(9);
    counter.setSystemErrorCounter(13);
    BulkUpdateServiceUtil
        .updateBulkStatusAborted(bulkProcess, STORE_ID, BULK_PROCESS_CODE, ERROR_MSG, counter);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(Integer.valueOf(9), Integer.valueOf(bulkProcess.getInputErrorCount()));
    Assertions.assertEquals(Integer.valueOf(13), Integer.valueOf(bulkProcess.getSystemErrorCount()));
    Assertions.assertEquals(STORE_ID, bulkProcess.getBulkProcessNotes().get(1).getStoreId());

  }

  @Test
  public void updateBulkStatusAbortedForBulkProcessNotesNullTest(){
    counter.setInputErrorCounter(9);
    counter.setSystemErrorCounter(13);
    bulkProcess.setBulkProcessNotes(null);
    BulkUpdateServiceUtil
        .updateBulkStatusAborted(bulkProcess, STORE_ID, BULK_PROCESS_CODE, ERROR_MSG, counter);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
    Assertions.assertEquals(Integer.valueOf(9), Integer.valueOf(bulkProcess.getInputErrorCount()));
    Assertions.assertEquals(Integer.valueOf(13), Integer.valueOf(bulkProcess.getSystemErrorCount()));
    Assertions.assertEquals(STORE_ID, bulkProcess.getBulkProcessNotes().get(0).getStoreId());
  }

  @Test
  public void updateBulkProcessNotesTest() throws Exception{
    listBulkUpdateErrorDTO.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    this.bulkUpdateServiceUtil
        .updateBulkProcessNotes(listBulkUpdateErrorDTO, STORE_ID, BULK_PROCESS_CODE, bulkProcess, false, null);
    Assertions.assertEquals(STORE_ID, bulkProcess.getBulkProcessNotes().get(1).getStoreId());
    Assertions.assertTrue(
        bulkProcess.getBulkProcessNotes().get(1).getNotes().toString().contains("harga error"));
    Assertions.assertTrue(bulkProcess.getBulkProcessNotes().get(2).getNotes().toString()
        .contains("harga  penjualan error"));
    Assertions.assertTrue(bulkProcess.getBulkProcessNotes().get(3).getNotes().toString()
        .contains("produck nama error"));
    Assertions.assertTrue(bulkProcess.getBulkProcessNotes().get(1).getNotes().toString().contains("MT-0121"));
    Assertions.assertTrue(bulkProcess.getBulkProcessNotes().get(2).getNotes().toString().contains("MT-0122"));
    Assertions.assertTrue(bulkProcess.getBulkProcessNotes().get(3).getNotes().toString().contains("MT-0123"));
  }

  private List<String> getPickupPointCodes() {
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add("PP-3000297");
    pickupPointCodes.add("PP-3000298");
    pickupPointCodes.add("PP-3000299");
    return pickupPointCodes;
  }

  @Test
  public void testPrepareUpdateSummaryRequestWithAmphiUser() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
    Assertions.assertNull(productLevel3UpdateSummaryRequest.getOff2OnActiveFlag());
  }

  @Test
  public void testPrepareUpdateSummaryRequestWithAmphiUserRemoveCheckMinDiscountCondition() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    productData.put(BulkParameters.SELLING_PRICE_HEADER, "10");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 100d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
    Assertions.assertNull(productLevel3UpdateSummaryRequest.getOff2OnActiveFlag());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForAmphiUserWithBuyableFalseAndDiscoverableTrue() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    productData.put(BulkParameters.AMPHI_SKU_STATUS, "2.0");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setOff2OnActiveFlag(true);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getOff2OnActiveFlag());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForAmphiUserWithBuyableTrueAndDiscoverableTrue() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    productData.put(BulkParameters.AMPHI_SKU_STATUS, "1.0");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForAmphiUserWithDecimalValueForBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    productData.put(BulkParameters.AMPHI_SKU_STATUS, "3.0");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForAmphiUserWithWrongDecimalValueForBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    productData.put(BulkParameters.AMPHI_SKU_STATUS, "2.1");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForAmphiUserWithEmptyValueForBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForExternalUserWithBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    productData.put(BulkParameters.EXTERNAL_SKU_STATUS, "0.0");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForExternalUserWithDecimalValueForBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    productData.put(BulkParameters.EXTERNAL_SKU_STATUS, "1.0");
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForExternalUserWithEmptyDecimalValueForBuyableAndDiscoverable() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    productData.put(BulkParameters.EXTERNAL_SKU_STATUS, StringUtils.EMPTY);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertFalse(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequestForExternalUser() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, true));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getBuyable());
    Assertions.assertTrue(productLevel3UpdateSummaryRequest.getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void testPrepareUpdateSummaryRequest_editStock_syncStockIsfalse() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setAvailableStockLevel2(50);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(
        Integer.valueOf(cleanDatas.get(0).get(BulkParameters.STOCK_HEADER))
            - productLevel3SummaryResponse.getAvailableStockLevel2(),
        productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
  }

  @Test
  public void testPrepareUpdateSummaryRequest_editStock_syncStockIsfalse_stockBlank() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.STOCK_HEADER, "");
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setAvailableStockLevel2(50);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(-productLevel3SummaryResponse.getAvailableStockLevel2(),
        productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
  }

  @Test
  public void testPrepareUpdateSummaryRequest_editStock_syncStockIsTrue() {
    cleanDatas = generateExcelData(1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    productLevel3SummaryResponse.setSynchronizeStock(true);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
  }

  @Test
  public void deletePassedProductFromExcelTest() throws Exception {
    Map<String, String> productRow1 = new HashMap<String, String>();
    Map<String, String> productRow2 = new HashMap<String, String>();
    productRow1.put("RowNumber", "0");
    productRow2.put("RowNumber", "1");
    List<Map<String, String>> validProductRows = new ArrayList();
    validProductRows.add(productRow1);
    validProductRows.add(productRow2);

    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(
        BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream,
        "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);

    BulkUpdateServiceUtil.deletePassedDataFromExcel(excelSheetData, validProductRows,
        BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
  }

  @Test
  public void deletePassedCampaignProductFromExcelTest() throws Exception {
    Map<String, String> campaignProductRow1 = new HashMap<String, String>();
    Map<String, String> campaignProductRow2 = new HashMap<String, String>();
    campaignProductRow1.put("RowNumber", "5");
    campaignProductRow2.put("RowNumber", "6");

    List<Map<String, String>> validCampainProductRows = new ArrayList();
    validCampainProductRows.add(campaignProductRow1);
    validCampainProductRows.add(campaignProductRow2);

    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkCampaignUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);

    BulkUpdateServiceUtil.deletePassedDataFromExcel(excelSheetData, validCampainProductRows,
        BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR);
  }

  @Test
  public void testDeletePassedInstantPickupProductFromExcel() throws Exception {
    Map<String, String> instantPickupProductRow1 = new HashMap<>();
    instantPickupProductRow1.put("RowNumber", "0");

    Map<String, String> instantPickupProductRow2 = new HashMap<>();
    instantPickupProductRow2.put("RowNumber", "1");

    List<Map<String, String>> validInstantPickupProductRows = new ArrayList<>();
    validInstantPickupProductRows.add(instantPickupProductRow1);
    validInstantPickupProductRows.add(instantPickupProductRow2);

    Map<String, String> files = this.getFiles("BulkUpsert" + File.separator + "BulkInstantPickupUpsert.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "test.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);

    BulkUpdateServiceUtil.deletePassedDataFromExcel(excelSheetData, validInstantPickupProductRows,
        BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
  }

  private Map<String, String> getFiles(String dirName) throws Exception {
    Map<String, String> files = new HashMap<>();
    String excelData = new String(
        Base64.encodeBase64(IOUtils.toByteArray(
            Thread.currentThread().getContextClassLoader().getResourceAsStream(dirName))),
        StandardCharsets.UTF_8);
    files.put("xlsx", excelData);
    return files;
  }

  @Test
  public void deletePassedProductfromUploadProductExcel() throws Exception{
    List<Integer> validProductRows = new ArrayList();
    validProductRows.add(2);

    Map<String, String> files =
        this.getFiles("ProductLevel3Processor" + File.separator + "ProductLevel3Processor.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath
        + File.separator + BULK_PROCESS_CODE
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath)
            .append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    BulkUpdateServiceUtil
        .deletePassedProductFromUploadProductExcel(fileInputStream, validProductRows, BULK_PROCESS_CODE,
            Constant.FILE_TYPE_XLSX);
  }

  @Test
  public void validateExcelFileForBulkArchiveTest() throws Exception {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId(STORE_ID);
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    InputStream fileInputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL),
            0);
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkArchive(excelSheetData, bulkProcess,
        bulkUpdateQueue, bulkUpdateErrorCounter);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateExcelFileForBulkArchiveTest_WithFailure() throws Exception {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId(STORE_ID);
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    InputStream fileInputStream = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("BulkUpdate" + File.separator + "InternalBulkUpdate.xlsx");
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL),
            0);
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkArchive(excelSheetData, bulkProcess,
        bulkUpdateQueue, bulkUpdateErrorCounter);
    Assertions.assertFalse(result);
  }

  private void prepareToValidateFinalPriceGreaterThanMinFinalPrice(BulkAddCampaignProductQueue bulkAddCampaignProductQueue, List<String> categories,List<ProductLevel3SummaryResponse> responses, boolean flag){
    bulkAddCampaignProductQueue.setCampaignCode(CAMPAIGN_CODE);
    bulkAddCampaignProductQueue.setStoreId(STORE_ID);
    bulkAddCampaignProductQueue.setRequestId(REQUEST_ID);
    bulkAddCampaignProductQueue.setUpdatedBy(USERNAME);
    bulkAddCampaignProductQueue.setMaxDiscount(Double.valueOf(5000));
    bulkAddCampaignProductQueue.setMinDiscount(Double.valueOf(500));
    bulkAddCampaignProductQueue.setMinFinalPrice(4000);
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();

    categories.add("categoryCode");
    campaignItemSummaryRequest.setCategories(categories);
    campaignItemSummaryRequest.setMerchantCode("merchantCode");
    List<String> brands = Collections.singletonList("Brand");
    campaignItemSummaryRequest.setBrands(brands);
    bulkAddCampaignProductQueue.setCampaignItemSummaryRequest(campaignItemSummaryRequest);

    ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
    response.setItemSku(DEFAULT_BLIBLI);
    ProductLevel3PriceResponse price = new ProductLevel3PriceResponse(CHANNEL_ID, 10000d, 10000d);
    if(!flag) {
      price.setSalePrice(8000d);
    }

    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(price);
    response.setPrices(prices);
    response.setBrand("Brand");
    response.setCategoryCode("categoryCode");
    response.setMerchantCode("merchantCode");

    responses.add(response);

    bulkProcess.setBusinessPartnerCode(DEFAULT_BLIBLI);
  }

  @Test
  public void authorizeUploadBulkUpdateWithEmptyExcelSheetTest() throws Exception {
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(
        BULK_PROCESS_CODE, "test1.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    configureExcelSheet(excelSheetData);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil
        .authorizeUploadBulkUpdate(privilegedMap, excelSheetData, bulkProcess, bulkUpdateQueue,
          counter, MerchantStatusType.PURE_DELIVERY, false, false);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertFalse(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheetTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheetTest_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files =
        this.getFiles("BulkUpdate" + File.separator + "BulkUpdate_cnc1P.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx",
            ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream,
        "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess,
            bulkUpdateQueue, counter, MerchantStatusType.PURE_DELIVERY, true, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheetTestAmphi_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files =
        this.getFiles("BulkUpdate" + File.separator + "BulkUpdateAmphi_cnc1P.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx",
            ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream,
        "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess,
            bulkUpdateQueue, counter, MerchantStatusType.PURE_DELIVERY, true, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheetTest4() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", false);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheet2Test() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    excelSheetData.getRow(0).getCell(0).setCellValue(String.valueOf(MerchantStatusType.PURE_DELIVERY));
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheet3Test() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    excelSheetData.getRow(0).getCell(9).setCellValue(String.valueOf(MerchantStatusType.PURE_DELIVERY));
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheet4Test() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    excelSheetData.getRow(0).getCell(9).setCellValue(BulkParameters.AMPHI_SKU_STATUS);
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheet4WithoutHeadersTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdateWithoutNewHeaders.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    excelSheetData.getRow(0).getCell(9).setCellValue(BulkParameters.AMPHI_SKU_STATUS);
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void authorizeUploadBulkUpdateExcelSheet4WithoutHeadersTest11() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdateWithoutNewHeaders.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    excelSheetData.getRow(0).getCell(9).setCellValue(BulkParameters.AMPHI_SKU_STATUS);
    Map<String, Boolean> privilegeMap = new HashMap<>(PRIVILEGE_MAP);
    privilegeMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(privilegeMap, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);
    Assertions.assertFalse(result);
  }

  private void setBulkUpdateQueue(BulkUpdateQueue bulkUpdateQueue) {
    bulkUpdateQueue.setStoreId(STORE_ID);
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkUpdateQueue.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  private void configureExcelSheet(Sheet excelSheetData) {
    excelSheetData.removeRow(excelSheetData.getRow(0));
    excelSheetData.removeRow(excelSheetData.getRow(1));
    excelSheetData.removeRow(excelSheetData.getRow(2));
  }

  private void setPrivilegedMap(Map<String, Boolean> privilegedMap) {
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
  }

  @Test
  public void testUpdateBulkProductFinalStatus() {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<String, Boolean>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    List<String> descriptions = Lists.newArrayList("description1", "description2", "description3");
    bulkUpdateServiceUtil.updateBulkProductFinalStatus(bulkProcess, bulkUpdateQueue, 1, 0,
        listBulkUpdateErrorDTO, counter, descriptions);
  }

  @Test
  public void testUpdateBulkCncProductFinalStatus() {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    List<String> descriptions = Lists.newArrayList("description1", "description2", "description3");
    bulkUpdateServiceUtil.updateBulkCncProductFinalStatus(bulkProcess, bulkUpdateQueue, 1, 0,
      bulkCncUpsertErrorDTOList, counter, descriptions);
  }

  private void testUpdateBulkDeleteCncProductFinalStatus(int savedProducts, int totalProducts) {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    List<String> descriptions = Lists.newArrayList("description1", "description2", "description3");
    bulkUpdateServiceUtil.updateBulkDeleteCncProductFinalStatus(bulkProcess, bulkUpdateQueue,
        savedProducts, totalProducts, bulkCncUpsertErrorDTOList, counter, descriptions);
  }

  @Test
  public void testUpdateBulkDeleteCncProductFinalStatusMpp() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(UUID.randomUUID().toString());
    bulkUpdateQueue.setBusinessPartnerCode("TOA-14961");
    bulkUpdateQueue.setBulkProcessType("Update");
    bulkUpdateQueue.setClientHost("localhost");
    bulkUpdateQueue.setFileName("test.xlsx");
    bulkUpdateQueue.setPrivilegedMap(new HashMap<>());
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setUpdatedBy("developer");

    List<String> descriptions = Lists.newArrayList("description1", "description2", "description3");
    bulkUpdateServiceUtil.updateBulkDeleteCncProductFinalStatus(bulkProcess, bulkUpdateQueue,
        2, 2, bulkCncUpsertErrorDTOList, counter, descriptions);
  }

  @Test
  public void testUpdateBulkDeleteCncProductFinalStatus_allSucceeded() {
    testUpdateBulkDeleteCncProductFinalStatus(2, 2);
  }

  @Test
  public void testUpdateBulkDeleteCncProductFinalStatus_partiallySucceeded() {
    testUpdateBulkDeleteCncProductFinalStatus(1, 2);
  }

  @Test
  public void testUpdateBulkDeleteCncProductFinalStatus_allFailed() {
    testUpdateBulkDeleteCncProductFinalStatus(0, 2);
  }

  private List<Map<String,String>> generateInstantPickupProductExcelData(int size){
    List<Map<String, String>> excelData = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      Map<String, String> instantPickupProductRow = new HashMap<>();
      instantPickupProductRow.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
      instantPickupProductRow.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
      instantPickupProductRow.put(BulkParameters.LIST_PRICE, "10000");
      instantPickupProductRow.put(BulkParameters.OFFER_PRICE, "5000");
      instantPickupProductRow.put(BulkParameters.STOCK, "1");
      excelData.add(instantPickupProductRow);
    }
    return excelData;
  }

  private List<Map<String,String>> generateDeleteInstantPickupProductExcelData(int size){
    List<Map<String, String>> excelData = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      Map<String, String> instantPickupProductRow = new HashMap<>();
      instantPickupProductRow.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
      instantPickupProductRow.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
      excelData.add(instantPickupProductRow);
    }
    return excelData;
  }

  @Test public void testValidateRowDataFromExcel_invalidBlibliSkuFormat() {
    cleanDatas = generateInstantPickupProductExcelData(1);
    BulkUpdateServiceUtil.validateRowDataFromExcel(cleanDatas);
    Assertions.assertEquals(1, cleanDatas.size());
  }

  @Test
  public void validateProductSkuWhenUploadFalseTest() {
    Assertions.assertFalse(BulkUpdateServiceUtil.validateProductSkuWhenUpload(ITEM_SKU));
  }

  @Test
  public void validateProductSkuWhenUploadTest() {
    Assertions.assertTrue(BulkUpdateServiceUtil.validateProductSkuWhenUpload(PRODUCT_SKU));
  }

  @Test
  public void testValidateRowDataFromExcel_validBlibliSkuFormat() {
    cleanDatas = generateInstantPickupProductExcelData(1);

    Map<String, String> invalidInstantPickupProductRow = new HashMap<>();
    invalidInstantPickupProductRow.put(BulkParameters.BLIBLI_SKU, DEFAULT_BLIBLI);
    invalidInstantPickupProductRow.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    invalidInstantPickupProductRow.put(BulkParameters.OFFER_PRICE, "5000");
    invalidInstantPickupProductRow.put(BulkParameters.STOCK, "1");

    cleanDatas.add(invalidInstantPickupProductRow);
    BulkUpdateServiceUtil.validateRowDataFromExcel(cleanDatas);
    Assertions.assertEquals(1, cleanDatas.size());
  }

  @Test
  public void testValidateItemSku_correctPattern() {
    Assertions.assertTrue(BulkUpdateServiceUtil.validateItemSku(ITEM_SKU));
  }

  @Test
  public void testValidateItemSku_incorrectPattern() {
    Assertions.assertFalse(BulkUpdateServiceUtil.validateItemSku(DEFAULT_BLIBLI));
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsert_emptyExcelSheet() throws Exception {
    Map<String, String> files =
      this.getFiles("BulkUpsert" + File.separator + "BulkInstantPickupUpsert.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
      directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
      excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "BulkInstantPickupUpsert.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    configureExcelSheet(excelSheetData);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertFalse(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertWithEnglishHeader() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-EN.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
      "MultiPickupPointTemplate-EN.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsert() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-IN.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate-IN.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertMultiPickupPointTemplate() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertMultiPickupPointTemplate_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"cncForWarehouseFeatureSwitch",true);
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate_cnc1P.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate_cnc1P.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertBlibliSkuWithExampleTemplate() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-IN-1.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate-IN-1.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertTemplate_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"cncForWarehouseFeatureSwitch", true);
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-EN_cnc1P.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate-EN_cnc1P.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertTemplate_cncForWarehouseOn_noStatus() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"cncForWarehouseFeatureSwitch", true);
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-EN_cnc1P_2.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate-EN_cnc1P_2.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertTrue(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkUpsertFailCase() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkUpsert" + File.separator + "MultiPickupPointTemplate-invalid.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "MultiPickupPointTemplate-invalid.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkUpsert(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter, new HashMap<>());
    Assertions.assertFalse(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkDelete_emptyExcelSheet() throws Exception {
    Map<String, String> files =
      this.getFiles("BulkDelete" + File.separator + "BulkInstantPickupDelete.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
      ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
      directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
      excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
        "BulkInstantPickupDelete.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    configureExcelSheet(excelSheetData);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter);
    verify(bulkProcessService).saveOperation(bulkProcess);
    Assertions.assertFalse(result);
  }

  @Test
  public void testAuthorizeUploadInstantPickupProductBulkDelete() throws Exception {
    Map<String, String> files =
        this.getFiles("BulkDelete" + File.separator + "BulkInstantPickupDelete.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath =
        ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(
        directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE,
      "BulkInstantPickupDelete.xlsx", ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR);
    Sheet excelSheetData = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.authorizeUploadInstantPickupProductBulkDelete(
        excelSheetData, bulkProcess, bulkUpdateQueue, counter);
    Assertions.assertTrue(result);
  }

  @Test
  public void fromBulkConfigurationUpdateRequestToBulkProcessTest() {
    BulkInternalProcess bulkProcess =
        BulkUpdateServiceUtil.getBulkProcessFromBulkConfigurationUpdateRequest(bulkConfigurationUpdateRequest, 0, 0);
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcess.getStatus());
  }

  @Test
  public void deletePassedProductfromUploadProductExcelwithXLSX() throws Exception{
    List<Integer> validProductRows = new ArrayList();
    validProductRows.add(2);
    Map<String, String> files =
        this.getFiles("ProductLevel3Processor" + File.separator + "bulk-product-template-upload.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSX));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath
        + File.separator + BULK_PROCESS_CODE
        + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath)
            .append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    BulkUpdateServiceUtil
        .deletePassedProductFromUploadProductExcel(fileInputStream, validProductRows, BULK_PROCESS_CODE,
            Constant.FILE_TYPE_XLSX);

  }

  @Test
  public void deletePassedProductfromUploadProductExcelwithXLS() throws Exception {
    List<Integer> validProductRows = new ArrayList();
    validProductRows.add(2);
    try {
      Map<String, String> files =
          this.getFiles("ProductLevel3Processor" + File.separator + "ProductLevel3Processor.xlsx");
      byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
      String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
      ProcessorUtils.createDirectories(directoryPath);
      ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
          excelFile);
      InputStream fileInputStream = new FileInputStream(new File(
          new StringBuilder(directoryPath).append(File.separator).append(BULK_PROCESS_CODE)
              .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
      Assertions.assertThrows(Exception.class,
          () ->  BulkUpdateServiceUtil
          .deletePassedProductFromUploadProductExcel(fileInputStream, validProductRows, BULK_PROCESS_CODE,
              Constant.FILE_TYPE_XLS));
    } catch (Exception e) {
    }
  }

  @Test
  public void testPrepareUpdateSummaryRequestSellerSKUNumericData() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, SELLER_SKU_1);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertEquals(SELLER_SKU_1, productLevel3UpdateSummaryRequest.getMerchantSku());
  }

  @Test
  public void testPrepareUpdateSummaryRequestSellerSKUDoubleData() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, SELLER_SKU_2);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertEquals(SELLER_SKU_2, productLevel3UpdateSummaryRequest.getMerchantSku());
  }


  @Test
  public void testPrepareUpdateSummaryRequestSellerSKUNumberStartingWith0Data() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, SELLER_SKU_4);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertEquals(SELLER_SKU_4, productLevel3UpdateSummaryRequest.getMerchantSku());
  }


  @Test
  public void testPrepareUpdateSummaryRequestSellerSKUAlphaNumericData() {
    cleanDatas = generateExcelData(1);
    cleanDatas.get(0).put(BulkParameters.SELLER_SKU, SELLER_SKU_3);
    ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest =
        new ProductLevel3UpdateSummaryRequest();
    List<ProductLevel3ViewConfigRequest> updateViewConfigs = new ArrayList<>();
    updateViewConfigs.add(new ProductLevel3ViewConfigRequest());
    productLevel3UpdateSummaryRequest.setViewConfigs(updateViewConfigs);
    List<ProductLevel3PriceRequest> updatePrices = new ArrayList<>();
    updatePrices.add(new ProductLevel3PriceRequest());
    productLevel3UpdateSummaryRequest.setPrices(updatePrices);
    Map<String, String> productData = cleanDatas.get(0);
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(null, false, false));
    productLevel3SummaryResponse.setViewConfigs(viewConfigs);
    List<ProductLevel3PriceResponse> prices = new ArrayList<>();
    prices.add(new ProductLevel3PriceResponse(null, 1000d, 500d, null, null, null, null));
    productLevel3SummaryResponse.setPrices(prices);
    this.bulkUpdateServiceUtil.prepareUpdateSummaryRequest(productLevel3UpdateSummaryRequest,
        productData, privilegedMap, productLevel3SummaryResponse);
    Assertions.assertEquals(0, productLevel3UpdateSummaryRequest.getDeltaStock().intValue());
    Assertions.assertEquals(SELLER_SKU_3, productLevel3UpdateSummaryRequest.getMerchantSku());
  }

  @Test
  public void validateExcelFileForBulkVatUpdateTest() throws Exception {
    Map<String, String> files = this.getFiles(BULK_VAT_DIRECTORY_1);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSX));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils
        .createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath).append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    Sheet worksheet = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    boolean validation = bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(worksheet, new BulkProcess(), new BulkUpdateQueue(),
        new BulkUpdateErrorCounter());
    Assertions.assertTrue(validation);
  }

  @Test
  public void validateExcelFileForBulkVatUpdateNoSkuCodeTest() throws Exception {
    Map<String, String> files = this.getFiles(BULK_VAT_DIRECTORY_2);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSX));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils
        .createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath).append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    Sheet worksheet = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    boolean validation = bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(worksheet, new BulkProcess(), new BulkUpdateQueue(),
        new BulkUpdateErrorCounter());
    Assertions.assertFalse(validation);
  }

  @Test
  public void validateExcelFileForBulkVatUpdateNoVatFlagTest() throws Exception {
    Map<String, String> files = this.getFiles(BULK_VAT_DIRECTORY_3);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSX));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils
        .createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath).append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    Sheet worksheet = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    boolean validation = bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(worksheet, new BulkProcess(), new BulkUpdateQueue(),
        new BulkUpdateErrorCounter());
    Assertions.assertFalse(validation);
  }

  @Test
  public void validateExcelFileForBulkVatUpdateNoHeaderTest() throws Exception {
    Map<String, String> files = this.getFiles(BULK_VAT_DIRECTORY_4);
    byte[] excelFile = Base64.decodeBase64(files.get(Constant.FILE_TYPE_XLSX));
    String directoryPath = ProcessorUtils.DATA_BASE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils
        .createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL, excelFile);
    InputStream fileInputStream = new FileInputStream(new File(
        new StringBuilder(directoryPath).append(File.separator).append(BULK_PROCESS_CODE)
            .append(ProcessorUtils.FILETYPE_XLSX_EXCEL).toString()));
    Sheet worksheet = POIUtil.getSheetForInputStream(fileInputStream, true, 0);
    boolean validation = bulkUpdateServiceUtil.validateExcelFileForBulkVatUpdate(worksheet, new BulkProcess(), new BulkUpdateQueue(),
        new BulkUpdateErrorCounter());
    Assertions.assertFalse(validation);
  }

  @Test
  public void validateVatUpdateDtoTest() {
    VatUpdateDto vatUpdateDto1 = new VatUpdateDto(ITEM_SKU, "1");
    VatUpdateDto vatUpdateDto2 = new VatUpdateDto(ITEM_SKU, "0");
    VatUpdateDto vatUpdateDto3 = new VatUpdateDto(StringUtils.EMPTY, "1");
    VatUpdateDto vatUpdateDto4 = new VatUpdateDto(ITEM_SKU, "true");

    boolean validation1 = bulkUpdateServiceUtil.validateVatUpdateDto(vatUpdateDto1);
    boolean validation2 = bulkUpdateServiceUtil.validateVatUpdateDto(vatUpdateDto2);
    boolean validation3 = bulkUpdateServiceUtil.validateVatUpdateDto(vatUpdateDto3);
    boolean validation4 = bulkUpdateServiceUtil.validateVatUpdateDto(vatUpdateDto4);

    Assertions.assertTrue(validation1);
    Assertions.assertTrue(validation2);
    Assertions.assertFalse(validation3);
    Assertions.assertFalse(validation4);
  }

  @Test
  public void updateBulkFinalStatusAndSendNotification() {
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, true, false, null);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO), false);
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO1 =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, false, false, null);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO1), false);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO, bulkUpdateSuccessDTO1), false);
    bulkUpdateServiceUtil
        .updateBulkFinalStatusAndSendNotification(bulkProcess, 5, Arrays.asList(bulkUpdateErrorDTO), Arrays.asList(),
            false);
  }

  @Test
  public void updateBulkFinalStatusAndSendNotification_IS() {
    BulkUpdateErrorDTO bulkUpdateErrorDTO =
        new BulkUpdateErrorDTO("Tes Product", "TOA-14961-00118-00001", "Harga Normal harus lebih besar dari 1");
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006",PICKUP_POINT_CODE, false,
          false, true, false, null);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO), true);
    BulkUpdateSuccessDTO bulkUpdateSuccessDTO1 =
        new BulkUpdateSuccessDTO("Tes Product", "TOA-14961-00117-00006", PICKUP_POINT_CODE, false
          , false, false, false, null);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO1), true);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, new ArrayList<>(),
        Arrays.asList(bulkUpdateSuccessDTO, bulkUpdateSuccessDTO1), true);
    bulkUpdateServiceUtil
        .updateBulkFinalStatusAndSendNotification(bulkProcess, 5, Arrays.asList(bulkUpdateErrorDTO), Arrays.asList(),
            true);
    bulkUpdateServiceUtil.updateBulkFinalStatusAndSendNotification(bulkProcess, 5, Arrays.asList(bulkUpdateErrorDTO),
        Arrays.asList(bulkUpdateSuccessDTO), true);
  }

  @Test
  public void getFailedSuccessDto() throws IOException {
    BulkProcessData bulkProcessData = BulkProcessData.builder().status(BulkProcessData.STATUS_SUCCESS).build();
    BulkProcessData bulkProcessData1 = BulkProcessData.builder().status(BulkProcessData.STATUS_FAIL).build();
    BulkProcessData bulkProcessData2 =
        BulkProcessData.builder().status(BulkProcessData.STATUS_FAIL).errorMessage(Constant.SYSTEM_ERROR).build();
    bulkUpdateServiceUtil.getFailedSuccessDto(new ArrayList<>(), bulkProcess, new ArrayList<>(),
        Arrays.asList(bulkProcessData));
    bulkUpdateServiceUtil.getFailedSuccessDto(new ArrayList<>(), bulkProcess, new ArrayList<>(),
        Arrays.asList(bulkProcessData1, bulkProcessData2));
  }

  @Test
  public void setBlpInitialDataForSuspension() throws Exception{
    BulkInternalProcessData bulkInternalProcessData = BulkInternalProcessData.builder().data("{\"data\":\"data\"}").build();
    bulkUpdateServiceUtil.getRowDataToProcessSuspension(Arrays.asList(bulkInternalProcessData));
  }

  @Test
  public void setBlpFinalDataForSuspension() throws Exception{
    BulkInternalProcessData bulkInternalProcessData = BulkInternalProcessData.builder().data("{\"data\":\"data\"}").build();
    bulkUpdateServiceUtil.setBlpFinalDataForSuspension(Arrays.asList(bulkInternalProcessData), new ArrayList<>());
    Map<String, String> failedMap = new HashMap<>();
    failedMap.put("code", PRODUCT_SKU);
    failedMap.put("reason", "SUSPEND");
    failedMap.put(BulkProductSuspensionParameters.PRODUCT_CODE, PRODUCT_SKU);
    bulkInternalProcessData.setParentCode(PRODUCT_SKU);
    bulkUpdateServiceUtil.setBlpFinalDataForSuspension(Arrays.asList(bulkInternalProcessData), Arrays.asList(failedMap));
  }

  @Test
  public void setBlpFinalDataForConfigUpdate() throws Exception {
    BulkInternalProcessData bulkInternalProcessData = BulkInternalProcessData.builder().data("{\"data\":\"data\"}")
        .processType(BulkConfigurationUpdateParameters.MERCHANT).build();
    bulkUpdateServiceUtil.setBlpFinalDataForSuspension(Arrays.asList(bulkInternalProcessData), new ArrayList<>());
    Map<String, String> failedMap = new HashMap<>();
    failedMap.put("code", PRODUCT_SKU);
    failedMap.put("reason", "SUSPEND");
    failedMap.put(BulkProductSuspensionParameters.PRODUCT_CODE, PRODUCT_SKU);
    bulkInternalProcessData.setParentCode(PRODUCT_SKU);
    bulkUpdateServiceUtil
        .setBlpFinalDataForConfigUpdate(Arrays.asList(bulkInternalProcessData), Arrays.asList(failedMap));
  }

  @Test
  public void setBlpFinalDataForConfigUpdate_Cate() throws Exception {
    BulkInternalProcessData bulkInternalProcessData = BulkInternalProcessData.builder().data("{\"data\":\"data\"}")
        .processType(BulkConfigurationUpdateParameters.CATEGORY).build();
    bulkUpdateServiceUtil.setBlpFinalDataForSuspension(Arrays.asList(bulkInternalProcessData), new ArrayList<>());
    Map<String, String> failedMap = new HashMap<>();
    failedMap.put("code", PRODUCT_SKU);
    failedMap.put("reason", "SUSPEND");
    failedMap.put(BulkProductSuspensionParameters.PRODUCT_CODE, PRODUCT_SKU);
    bulkInternalProcessData.setParentCode(PRODUCT_SKU);
    bulkUpdateServiceUtil
        .setBlpFinalDataForConfigUpdate(Arrays.asList(bulkInternalProcessData), Arrays.asList(failedMap));
  }

  @Test
  public void validateDataForPickupUpsertTest() {
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "5000.0");
    row.put("Harga Normal", "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "1.0");
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = bulkUpdateServiceUtil.validateDataForPickupUpsert(row, false);
    Assertions.assertNull(bulkCncUpsertErrorDTO);
  }

  @Test
  public void validateDataForPickupUpsert_inaccessiblePickupPoint_noPickupPointDataTest() {
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "5000.0");
    row.put("Harga Normal", "10000.0");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "1.0");
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = bulkUpdateServiceUtil.validateDataForPickupUpsert(row, false);
    Assertions.assertNull(bulkCncUpsertErrorDTO);
  }

  @Test
  public void validateDataForPickupUpsert_ValidationFailedTest() {
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_IN, StringUtils.EMPTY);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "-1");
    row.put("Harga Normal", "-1");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "-1");
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = bulkUpdateServiceUtil.validateDataForPickupUpsert(row, false);
    Assertions.assertNotNull(bulkCncUpsertErrorDTO);
    Assertions.assertEquals(ITEM_SKU, bulkCncUpsertErrorDTO.getItemSku());
    Assertions.assertEquals(StringUtils.EMPTY, bulkCncUpsertErrorDTO.getPickupPointCode());
  }

  @Test
  public void validateDataForPickupUpsertInternationalMerchantTest() {
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "5000.0");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "10000.0");
    row.put(ExcelHeaderNames.STOCK_EN, "1.0");
    row.put(ExcelHeaderNames.DELIVERY_STATUS_EN, "1");
    row.put(ExcelHeaderNames.CNC_STATUS_EN, "1.0");
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = bulkUpdateServiceUtil.validateDataForPickupUpsert(row, true);
    Assertions.assertNull(bulkCncUpsertErrorDTO);
  }

  @Test
  public void validateDataForPickupUpsert_InternationalMerchant_ValidationFailedTest() {
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(ExcelHeaderNames.PICKUP_POINT_CODE_EN, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put(ExcelHeaderNames.OFFER_PRICE_EN, "-1");
    row.put(ExcelHeaderNames.LIST_PRICE_EN, "-1");
    row.put(ExcelHeaderNames.STOCK_EN, "-1");
    BulkCncUpsertErrorDTO bulkCncUpsertErrorDTO = bulkUpdateServiceUtil.validateDataForPickupUpsert(row, true);
    Assertions.assertNotNull(bulkCncUpsertErrorDTO);
    Assertions.assertEquals(ITEM_SKU, bulkCncUpsertErrorDTO.getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, bulkCncUpsertErrorDTO.getPickupPointCode());
  }

  @Test
  public void getFinalDataListTest() throws Exception {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "-1");
    row.put("Harga Normal", "-1");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "-1");
    Map<String, List<BulkProcessData>> dataMap = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    dataMap.put(PARENT_1, Arrays.asList(bulkProcessData));
    bulkProcessData.setBulkRequestData("");
    dataMap.put(PARENT_2, Arrays.asList(bulkProcessData1, bulkProcessData2));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    List<BulkProcessData> list = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(dataMap, list, true, false);
    Assertions.assertEquals(3, errorCount);
    Assertions.assertEquals(3, list.size());
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(Mockito.any());
  }

  @Test
  public void getFinalDataListIsBulkUpsertTrueTest() throws Exception {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "-1");
    row.put("Harga Normal", "-1");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "-1");
    Map<String, List<BulkProcessData>> dataMap = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setBulkRequestData("");
    dataMap.put(PARENT_1, Arrays.asList(bulkProcessData));
    dataMap.put(PARENT_2, Arrays.asList(bulkProcessData1, bulkProcessData2));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    List<BulkProcessData> list = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(dataMap, list, false, true);
    Assertions.assertEquals(3, errorCount);
    Assertions.assertEquals(3, list.size());
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(Mockito.any(BulkCncUpsertErrorDTO.class));
  }

  @Test
  public void getFinalDataListPartialTest() throws Exception {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "-1");
    row.put("Harga Normal", "-1");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "-1");
    Map<String, List<BulkProcessData>> dataMap = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkRequestData("");
    bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
    dataMap.put(PARENT_1, Arrays.asList(bulkProcessData));
    dataMap.put(PARENT_2, Arrays.asList(bulkProcessData1, bulkProcessData2));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    List<BulkProcessData> list = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(dataMap, list, false, false);
    Assertions.assertEquals(2, errorCount);
    Assertions.assertEquals(3, list.size());
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(Mockito.any(BulkCncUpsertErrorDTO.class));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithFailAndSuccessMPP() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", false);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(this.productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(),
      anyString(), anyList())).thenReturn(childCategories);


    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);
    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(3);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    cleanDatas.get(2).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithPricingEnabled() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    itemSummaryListResponse.setCncActive(true);
    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    ItemViewConfigDTO itemViewConfig = new ItemViewConfigDTO();
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(false);
    itemViewConfigs.add(itemViewConfig);
    itemSummaryListResponse.setItemViewConfigs(itemViewConfigs);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);


    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(3);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    cleanDatas.get(2).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithPricingEnabledNonCNC() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    itemSummaryListResponse.setCncActive(true);
    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    ItemViewConfigDTO itemViewConfig = new ItemViewConfigDTO();
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(true);
    itemViewConfigs.add(itemViewConfig);
    itemSummaryListResponse.setItemViewConfigs(itemViewConfigs);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);
    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(3);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    cleanDatas.get(2).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithPureCNCPricingEnabledNonCNC() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    itemSummaryListResponse.setCncActive(true);
    itemSummaryListResponse.setItemViewConfigs(Collections.EMPTY_SET);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(3);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    cleanDatas.get(2).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductCleanData_WithPricingEnabledTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    itemSummaryListResponse.setCncActive(true);
    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    ItemViewConfigDTO itemViewConfig = new ItemViewConfigDTO();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(false);
    itemViewConfigs.add(itemViewConfig);
    itemSummaryListResponse.setItemViewConfigs(itemViewConfigs);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);


    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);

    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(3);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE);
    cleanDatas.get(1).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    cleanDatas.get(2).put(BulkParameters.PICKUP_POINT_CODE,PICKUP_POINT_CODE_1);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(0, failureDatas.size());
    Assertions.assertEquals(1, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
  }


  @Test
  public void validateExcelDatasBulkUpdateCampaignProductSkuNotPresentMPPTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(this.productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(),
      anyString(), anyList())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    campaignProductDetailResponses.get(0).setItemSku(ITEM_SKU);
    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);
    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
  }

  @Test
  public void quotaUsedMoreMppOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
      Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    campaignProductDetailResponses.get(0).setUsedQuota(100);
    when(xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
      failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
      anyBoolean());
    verify(xProductOutboundService)
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    verify(this.campaignRepository)
      .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
        bulkAddCampaignProductQueue);
  }

  @Test
  public void quotaUsedMoreMppOnExceptionTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    campaignProductDetailResponses.get(0).setUsedQuota(100);
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
        .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
            bulkAddCampaignProductQueue)).thenThrow(ApplicationRuntimeException.class);
    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> result =  bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
        anyBoolean());
    verify(this.campaignRepository)
        .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
            bulkAddCampaignProductQueue);
    Assertions.assertEquals(DEFAULT_BLIBLI, result.get(0).getProductSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void quotaUsedMoreMppOnTest2() throws Exception {
    bulkAddCampaignProductQueue.setMinQuota(1);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "GET_BATCH_SIZE", 2);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    campaignProductDetailResponses.get(0).setUsedQuota(100);
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
        .getCampaignProductDetailsV2(anyList(), Mockito.any())).thenReturn(campaignProductDetailResponses);
    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.BLIBLI_SKU, DEFAULT_BLIBLI);
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    cleanDatas.add(new HashMap<>());
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(2, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
        anyBoolean());
    verify(this.campaignRepository)
        .getCampaignProductDetailsV2(anyList(), Mockito.any());
  }

  @Test
  public void quotaUsedMoreMppOnEmptyItemListTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "GET_BATCH_SIZE", 2);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);

    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
        any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);

    List<String> itemSkuList = new ArrayList<>();

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    campaignProductDetailResponses.get(0).setUsedQuota(100);
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    when(this.campaignRepository
        .getCampaignProductDetailsV2(Collections.singletonList(itemDetailsDto),
            bulkAddCampaignProductQueue)).thenReturn(campaignProductDetailResponses);
    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(),
        anyBoolean());
  }

  @Test
  public void validateExcelDatasBulkUpdateCampaignProductInvalidBrandPricingMppFailure() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "pricingMultiPickupPointEnabled", true);
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    List<String> childCategories =
        Lists.newArrayList("categoryCode", "categoryCode2", "categoryCode3");
    getProductLevel3Responses(responses);
    itemPickupPointRequest.setPickupPointCode(null);
    responses.get(0).setBrand(null);
    when(pcbOutboundServiceBean.getAllChildCategoriesFromC1CategoryCode(anyString(),
      any(CategoryCodeRequest.class), anyBoolean())).thenReturn(childCategories);
    when(this.productCategoryBaseRepository.getAllChildCategoriesFromC1CategoryCode(anyString(),
        anyString(), anyList())).thenReturn(childCategories);

    List<String> itemSkuList = Lists.newArrayList(DEFAULT_BLIBLI);

    List<CampaignProductDetailResponse> campaignProductDetailResponses = getCampaignProductDetailResponses();
    when(xProductOutboundService
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest)))
        .thenReturn(Collections.singletonList(itemSummaryListResponse));

    cleanDatas = generateExcelProductCampaignData(1);
    cleanDatas.get(0).put(BulkParameters.KUOTA, "5");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    bulkUpdateServiceUtil.validateExcelDatasBulkUpdateCampaignProduct(cleanDatas, successDatas,
        failureDatas, counter, bulkAddCampaignProductQueue);
    Assertions.assertEquals(1, failureDatas.size());
    Assertions.assertEquals(0, successDatas.size());
    Assertions.assertEquals(0, counter.getBlibliSkuCounter());
    verify(xProductOutboundService)
        .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    verify(pcbOutboundServiceBean).getAllChildCategoriesFromC1CategoryCode(anyString(),any(
      CategoryCodeRequest.class),anyBoolean());
    verify(this.campaignRepository).getCampaignProductDetailsV2(anyList(),
        Mockito.eq(bulkAddCampaignProductQueue));
  }

  @Test
  public void getFinalDataListMppOnTest() throws Exception {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData("");
    bulkProcessData2.setBulkRequestData("");
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "multiPickupPointEnabled", true);
    Map<String, String> row = new LinkedHashMap<>();
    row.put(BulkParameters.BLIBLI_SKU, ITEM_SKU);
    row.put(BulkParameters.PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    row.put("RowNumber", "1");
    row.put("Harga Jual", "-1");
    row.put("Harga Normal", "-1");
    row.put("Nama Produk", "TOA A");
    row.put("Nama Toko/Gudang", "PP A");
    row.put("Stok", "-1");
    Map<String, List<BulkProcessData>> dataMap = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkRequestData("");
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    dataMap.put(PARENT_1, Arrays.asList(bulkProcessData));
    dataMap.put(PARENT_2, Arrays.asList(bulkProcessData2, bulkProcessData2));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), Mockito.any(TypeReference.class))).thenReturn(row);
    List<BulkProcessData> list = new ArrayList<>();
    int errorCount = bulkUpdateServiceUtil.getFinalDataList(dataMap, list, true, false);
    Assertions.assertEquals(3, errorCount);
    Assertions.assertEquals(3, list.size());
    verify(objectMapper, times(2)).readValue(Mockito.anyString(), Mockito.any(TypeReference.class));
    verify(objectMapper, times(2)).writeValueAsString(Mockito.any());
  }

  @Test
  public void validateExcelDatasBulkUpdateProductWithNaNPriceCounter() throws Exception{
    cleanDatas = generateExcelData(5);
    cleanDatas.get(0).put(BulkParameters.PRICE_HEADER, "3000");
    cleanDatas.get(0).put(BulkParameters.SELLING_PRICE_HEADER, "NaN");
    cleanDatas.get(1).put(BulkParameters.PRICE_HEADER, "NaN");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTO = bulkUpdateServiceUtil
      .validateExcelDatasBulkUpdateProduct(cleanDatas, pickupPointCodes, successDatas,
        failureDatas, counter, MINIMUM_PRICE, null, false, StringUtils.EMPTY);
    Assertions.assertEquals(2,bulkUpdateErrorDTO.size());
    Assertions.assertEquals(2,failureDatas.size());
    Assertions.assertEquals(3,successDatas.size());
    Assertions.assertEquals(1,counter.getHargaCounter());
  }

  @Test
  public void validateDuplicateFbbPickupPointsTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_1).build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_2).build();

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse1), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse2), PageRequest.of(1, 1), 2));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), true, null);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));

    Assertions.assertEquals(1, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsForWhEnabledTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_1).build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_2).build();

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse1), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse2), PageRequest.of(1, 1), 2));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), true, profileResponse);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));

    Assertions.assertEquals(1, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsMppWhiteListedTrueTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "mppAllowedSellers", "TD");
    profileResponse.getCompany().setCncActivated(true);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_1).build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_2).build();

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse1), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse2), PageRequest.of(1, 1), 2));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), true, profileResponse);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));

    Assertions.assertEquals(1, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsMppForWhEnabledAndWhitelistedTrueTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "mppAllowedSellers", "TD");
    profileResponse.getCompany().setCncActivated(true);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_1).build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_2).build();

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse1), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse2), PageRequest.of(1, 1), 2));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), true, profileResponse);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));

    Assertions.assertEquals(0, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsUpdateTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);
    PickupPointResponse pickupPointResponse1 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_1).build();
    PickupPointResponse pickupPointResponse2 = PickupPointResponse.builder().code(FBB_ITEM_PP_CODE_2).build();

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, FBB_ITEM_SKU_1, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse1), PageRequest.of(0, 1), 2));
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1),
            Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(pickupPointResponse2), PageRequest.of(1, 1), 2));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), false, null);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(0), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));
    Mockito.verify(businessPartnerRepository)
        .filterBusinessPartnerPickupPointV2(Mockito.eq(1), Mockito.eq(1), Mockito.any(PickupPointFilterRequest.class));

    Assertions.assertEquals(1, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsNoDuplicateTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "fbbPickupPointFetchSize", 1);
    String json1 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    String json2 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_2, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    String json3 = mapper.writeValueAsString(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_3, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    bulkProcessData1.setBulkRequestData(json1);
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkRequestData(json2);
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkRequestData(json3);

    Mockito.when(objectMapper.readValue(Mockito.eq(json1), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_1, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_1));
    Mockito.when(objectMapper.readValue(Mockito.eq(json2), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_2, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            FBB_ITEM_PP_CODE_2));
    Mockito.when(objectMapper.readValue(Mockito.eq(json3), Mockito.any(TypeReference.class))).thenReturn(
        ImmutableMap.of(ExcelHeaderNames.BLIBLI_SKU_EN, FBB_ITEM_SKU_3, ExcelHeaderNames.PICKUP_POINT_CODE_EN,
            PICKUP_POINT_CODE));

    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE,
        Arrays.asList(bulkProcessData1, bulkProcessData2, bulkProcessData3), true, null);

    Mockito.verify(objectMapper).readValue(Mockito.eq(json1), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json2), Mockito.any(TypeReference.class));
    Mockito.verify(objectMapper).readValue(Mockito.eq(json3), Mockito.any(TypeReference.class));
    Assertions.assertEquals(0, errorCount);
  }

  @Test
  public void validateDuplicateFbbPickupPointsEmptyDataListTest() throws Exception {
    int errorCount = bulkUpdateServiceUtil.validateDuplicateFbbPickupPoints(DEFAULT_BUSINESS_PARTNER_CODE, new ArrayList<>(), true, null);
    Assertions.assertEquals(0, errorCount);
  }

  @Test
  public void prepareUpdateL5SummaryRequestTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.SELLER_SKU, "merchantSku");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(true);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));

    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);

    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());

    Assertions.assertEquals(ITEM_SKU, productVariantUpdateRequest.getDeletePickupPoints().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, productVariantUpdateRequest.getDeletePickupPoints().get(0).getPickupPointId());
  }

  @Test
  public void prepareUpdateL5SummaryRequestFaasSellerTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.SELLER_SKU, "merchantSku");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);
    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());
    Assertions.assertEquals(ITEM_SKU, productVariantUpdateRequest.getDeletePickupPoints().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_1,
        productVariantUpdateRequest.getDeletePickupPoints().get(0).getPickupPointId());
  }

  @Test
  public void prepareUpdateL5SummaryRequestFaasSellerSwitchOnTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.SELLER_SKU, "merchantSku");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);
    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());
    Assertions.assertEquals(ITEM_SKU, productVariantUpdateRequest.getDeletePickupPoints().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_1,
        productVariantUpdateRequest.getDeletePickupPoints().get(0).getPickupPointId());
  }

  @Test
  public void prepareUpdateL5SummaryRequestTest_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint",
        true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku"
        ,BulkParameters.CNC_STATUS_HEADER,"1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(true);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
                .fbbActivated(true).waitingDeletion(false).build()))
        .thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));

    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);

    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).waitingDeletion(false).build());

    Assertions.assertEquals(ITEM_SKU,
        productVariantUpdateRequest.getDeletePickupPoints().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_1,
        productVariantUpdateRequest.getDeletePickupPoints().get(0).getPickupPointId());
  }

  @Test
  public void prepareUpdateL5SummaryRequestTest_cncForWarehouseOn_invalidStatus() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint",
        true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(true);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
                .fbbActivated(true).waitingDeletion(false).build()))
        .thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);
    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).waitingDeletion(false).build());
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_cncNotOffline() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "1.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true);
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_deliveryNotOffline() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "1.0",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true);
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_statusOffline() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "0.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true);
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_statusOffline_amphiUser() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "0.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, false);
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_statusOnline_amphiUser() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);

    Assertions.assertThrows(RuntimeException.class,
        () -> bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true));
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_cncNotOffline_amphiUser() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "1.0", BulkParameters.DELIVERY_STATUS_HEADER, "0.0",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true);
  }

  @Test
  public void checkStatusCombination_cncForWarehouseOn_deliveryNotOffline_amphiUser() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.SELLER_SKU, "merchantSku",
            BulkParameters.CNC_STATUS_HEADER, "0.0", BulkParameters.DELIVERY_STATUS_HEADER, "1.0",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);

    bulkUpdateServiceUtil.checkStatusCombination(privilegedMap, productL5Data, true);
  }

  @Test
  public void prepareUpdateL5SummaryRequestNoFbbPPCodeTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));

    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);

    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());

    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestNoFbbPPCodePrivilegedTest() throws Exception {
    try {
      ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
      ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
      Map<String, String> productL5Data =
          ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
              BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
              "1");
      Map<String, Boolean> privilegedMap = new HashMap<>();
      setPrivilegedMap(privilegedMap);
      privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
      ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
      itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
      itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
      itemPickupPointListingL3Response.setFbbActivated(false);

      Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
          PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
              .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
      ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
      itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
      Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
          .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
      privilegedMap.remove(BulkParameters.PRIVILEGE_EDIT_PRICE);
      bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
          true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
          profileResponse, null);

      Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
          PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
              .waitingDeletion(false).build());

      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    } catch (ApplicationException e) {
      Assertions.assertEquals("AUTHORIZATION", e.getErrorCodes().toString());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestNoFbbPPCodeNotPricEditTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
  try {
    Assertions.assertThrows(ApplicationException.class,
        () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
            productL5Data, privilegedMap, null, true, false,
            ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
            profileResponse, null));
  } finally {
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  }

  @Test
  public void prepareUpdateL5SummaryRequestNoFbbPPCodeAllMultipleFbbTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "mppForWhEnabled", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(Arrays.asList(new PickupPointResponse())));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));

    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);

    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());

    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestNewNonFbbPPCodeTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));

    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap, null,
        true, false, ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(),
        profileResponse, null);

    Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build());

    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueTestStockIsNotNumberTest() throws Exception {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "-", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 = new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueTest() throws Exception {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    profileResponse.setFlags(Map.of(ProfileFlagNames.BLIBLI_OMG,true));
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 = new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTrueTest() throws Exception {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTrueFaasSellerTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTrueFaasSellerSwitchOnTest() throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse=new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true,ProfileFlagNames.BLIBLI_OMG,true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID,PRODUCT_SKU)).thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTruePreOrderOMGSellerSwitchOnTest() throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse=new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true,ProfileFlagNames.BLIBLI_OMG,true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID,PRODUCT_SKU)).thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }
  @Test
  public void prepareUpdateL5SummaryRequestPreOrderEndedTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }
  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTrueNonFaasSellerSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueWebsSyncStockTrueFaasSellerNotEditTest() throws Exception {
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueTest_cncForWarehouseTrue_cncTrue()
      throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    List<com.gda.mta.product.dto.ItemPickupPointRequest> modifiedItemPickupPoints =
        new ArrayList<>();
    modifiedItemPickupPoints.add(itemPickupPointRequest1);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(modifiedItemPickupPoints);
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data,
        privilegedMap, itemPickupPointListingL3Response, false, false,
        ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(), profileResponse, null);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueTest_cncForWarehouseFalse_cncFalse()
      throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    List<com.gda.mta.product.dto.ItemPickupPointRequest> modifiedItemPickupPoints =
        new ArrayList<>();
    modifiedItemPickupPoints.add(itemPickupPointRequest1);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(modifiedItemPickupPoints);
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    productL5Data = new HashMap<>(productL5Data);
    productL5Data.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data,
        privilegedMap, itemPickupPointListingL3Response, false, false,
        ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(), profileResponse, null);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestMPPTrueTest_availableStockNull()
      throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    //availableStockLevel2 is null
    itemPickupPointListingL3Response.setAvailableStockLevel2(null);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    List<com.gda.mta.product.dto.ItemPickupPointRequest> modifiedItemPickupPoints =
        new ArrayList<>();
    modifiedItemPickupPoints.add(itemPickupPointRequest1);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(modifiedItemPickupPoints);
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    productL5Data = new HashMap<>(productL5Data);
    productL5Data.put(BulkParameters.STOCK_REMINDER_COLUMN_ID, "1");
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data,
        privilegedMap, itemPickupPointListingL3Response, false, false,
        ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response), new HashSet<>(), profileResponse, null);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequestNewNonFbbPPCodeInvalidStockTest() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "setWaitingDeletionForDeletePickupPoint" , true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
            ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
                    BulkParameters.STOCK_HEADER, "-", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
                    "1");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);

    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true)
            .waitingDeletion(false).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, null, true, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, null));

//      Mockito.verify(businessPartnerRepository).filterBusinessPartnerPickupPointV2(0, 1,
//          PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
//              .fbbActivated(true).build());
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());

  }

  @Test
  public void validateValidDataForBfbFieldsTest(){
    cleanDatas = generateExcelData(1);
    Map<String, String> cleanData = cleanDatas.get(0);
    cleanData.put(BulkParameters.BFB_BASE_PRICE,"100");
    boolean result = bulkUpdateServiceUtil.validateDataForBfbFields(cleanData,
      new StringBuilder(StringUtils.EMPTY),MINIMUM_PRICE);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateNegativeBfbPriceTest(){
    cleanDatas = generateExcelData(1);
    Map<String, String> cleanData = cleanDatas.get(0);
    cleanData.put(BulkParameters.BFB_BASE_PRICE,"-100");
    boolean result = bulkUpdateServiceUtil.validateDataForBfbFields(cleanData,
      new StringBuilder(StringUtils.EMPTY),MINIMUM_PRICE);
    Assertions.assertFalse(result);
  }

  @Test
  public void validateNaNBfbPriceTest(){
    cleanDatas = generateExcelData(1);
    Map<String, String> cleanData = cleanDatas.get(0);
    cleanData.put(BulkParameters.BFB_BASE_PRICE,"NaN");
    boolean result = bulkUpdateServiceUtil.validateDataForBfbFields(cleanData,
      new StringBuilder(StringUtils.EMPTY),MINIMUM_PRICE);
    Assertions.assertFalse(result);
  }

  @Test
  public void validateInvalidPriceBfbTest(){
    cleanDatas = generateExcelData(1);
    Map<String, String> cleanData = cleanDatas.get(0);
    cleanData.put(BulkParameters.BFB_BASE_PRICE,"abb");
    boolean result = bulkUpdateServiceUtil.validateDataForBfbFields(cleanData,
      new StringBuilder(StringUtils.EMPTY),MINIMUM_PRICE);
    Assertions.assertFalse(result);
  }

  @Test
  public void validateNoBfbPriceTest(){
    cleanDatas = generateExcelData(1);
    Map<String, String> cleanData = cleanDatas.get(0);
    boolean result = bulkUpdateServiceUtil.validateDataForBfbFields(cleanData,
      new StringBuilder(StringUtils.EMPTY),MINIMUM_PRICE);
    Assertions.assertTrue(result);
  }
  @Test
  public void setDisplayBuyableL5ExternalUserTest() {
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
      new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
      ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
        PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
        BulkParameters.SELLING_PRICE_HEADER, "1");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5(getPrivilegedMap(), productL5Data,
      itemPickupPointRequest1, itemPickupPointListingL3Response);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserNullViewConfigTest() {
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
      new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
      ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
        PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
        BulkParameters.SELLING_PRICE_HEADER, "1");

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    bulkUpdateServiceUtil.setDisplayBuyableL5(getPrivilegedMap(), productL5Data,
      itemPickupPointRequest1, itemPickupPointListingL3Response);
  }


  @Test
  public void setDisplayBuyableL5ExternalUserNullViewConfigPrivilageTest() {
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1");
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.remove(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    bulkUpdateServiceUtil.setDisplayBuyableL5(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseTest() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER,false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
      new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
      ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
        PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
        BulkParameters.SELLING_PRICE_HEADER, "1");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5(privilegedMap, productL5Data,
      itemPickupPointRequest1, itemPickupPointListingL3Response);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseNullViewConfigTest() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER,false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
      new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
      ImmutableMap.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
        PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
        BulkParameters.SELLING_PRICE_HEADER, "1");

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    bulkUpdateServiceUtil.setDisplayBuyableL5(privilegedMap, productL5Data,
      itemPickupPointRequest1, itemPickupPointListingL3Response);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserTest_cncForWarehouseTrue() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(getPrivilegedMap(), productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5CncForWarehouseOnUserTest_cncForWarehouseFalse() {
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(getPrivilegedMap(), productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5CncForWarehouseOnUserTest_cncForWarehouseTrue_deliveryFalse() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(getPrivilegedMap(), productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, false);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserNullViewConfigTest_cncForWarehouseTrue() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(getPrivilegedMap(), productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseTest_cncForWarehouseTrue() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER,false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseTest_cncForWarehouseTrue_deliveryFalse() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseTest_cncForWarehouseFalse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setDisplayBuyableL5ExternalUserFalseNullViewConfigTest_cncForWarehouseTrue() {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "cncForWarehouseFeatureSwitch", true);
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER,false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    bulkUpdateServiceUtil.setDisplayBuyableL5CncForWarehouseOn(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response, true);
  }

  @Test
  public void setBuyableDisplayForExternalTest() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternal(itemPickupPointRequest1,
        BulkParameters.OFFLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayForExternalTest_Online() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternal(itemPickupPointRequest1,
        BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphiTest() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.OFFLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphiTest_Teaser() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.TEASER_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphiTest_B2B() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.B2B_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_nullResponse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.DELIVERY_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));

    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1, null,
        BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_freeSample() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(true);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphi(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
  }

  @Test
  public void setDisplayBuyableL5CncChannel_Test() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "1.0", BulkParameters.CNC_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setDisplayBuyableL5CncChannel_Test_ExternalOnly() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1.0", BulkParameters.CNC_STATUS_HEADER,
            "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setDisplayBuyableL5CncChannel_Test_Offline() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.AMPHI_SKU_STATUS_CNC_1P, "1.0", BulkParameters.CNC_STATUS_HEADER, "0.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setDisplayBuyableL5CncChannel_Test_ExternalOnly_Offline() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, "1.0", BulkParameters.CNC_STATUS_HEADER,
            "0.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setDisplayBuyableL5CncChannel_Test_NotPriviledgedToEdit() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(viewConfigs);
    bulkUpdateServiceUtil.setDisplayBuyableL5CncChannel(privilegedMap, productL5Data,
        itemPickupPointRequest1, itemPickupPointListingL3Response);
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayForExternalTest_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternalForCnc(itemPickupPointRequest1,
        BulkParameters.OFFLINE_VALUE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayForExternalTest_ForCnc_nullProductResponse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternalForCnc(itemPickupPointRequest1,
        BulkParameters.OFFER_PRICE, null);
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayForExternalTest_ForCnc_nonNullProductResponse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternalForCnc(itemPickupPointRequest1,
        BulkParameters.OFFER_PRICE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayForExternalTest_Online_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));
    bulkUpdateServiceUtil.setBuyableDisplayForExternalForCnc(itemPickupPointRequest1,
        BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_CncTest() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.OFFLINE_VALUE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphiTest_Teaser_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.TEASER_VALUE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphiTest_B2B_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.B2B_VALUE, viewConfigs.get(0));
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_ForCnc_nullProductResponse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.OFF2ON_VALUE, null);
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_ForCnc_nonNullProductResponse() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(false);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.OFF2ON_VALUE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_nullResponse_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.CNC, false, false));

    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1, null,
        BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
    Assertions.assertTrue(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertTrue(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBuyableDisplayableForAmphi_Online_freeSample_ForCnc() {
    Map<String, Boolean> privilegedMap = getPrivilegedMap();
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();

    List<ProductLevel3ViewConfigResponse> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ProductLevel3ViewConfigResponse(Constant.DEFAULT, false, false));
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
        new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setFreeSample(true);
    bulkUpdateServiceUtil.setBuyableDisplayableForAmphiForCnc(itemPickupPointRequest1,
        itemPickupPointListingL3Response, BulkParameters.ONLINE_VALUE, viewConfigs.get(0));
    Assertions.assertFalse(itemPickupPointRequest1.isCncBuyable());
    Assertions.assertFalse(itemPickupPointRequest1.isCncDisplay());
  }

  @Test
  public void setBfbFieldsNoPrivilegeTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      Arrays.asList(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    itemPickupPointListingL3Response.setB2bFields(B2BResponse.builder().managed(true).build());
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserOfflineTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "0.0");
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "1");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserOnlineTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "1.0");
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "0.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertFalse(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsNullItemPickupPointREsponseTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "1.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0), null);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertFalse(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserTeaserTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    itemPickupPointListingL3Response.setFreeSample(true);
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "0");
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "2.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertFalse(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserB2BTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    itemPickupPointListingL3Response.setFreeSample(true);
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "3.0");
    itemPickupPointListingL3Response.setB2bFields(B2BResponse.builder().managed(true).build());
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserInvalidStatusB2BTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    itemPickupPointListingL3Response.setFreeSample(true);
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "5.0");
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "1.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsExternalUserOfflineTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    cleanDatas.get(0).put(BulkParameters.EXTERNAL_BFB_STATUS, "0.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
  }

  @Test
  public void setBfbFieldsExternalUserOnlineTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    itemPickupPointListingL3Response.setB2bFields(B2BResponse.builder().managed(true).build());
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    cleanDatas.get(0).put(BulkParameters.EXTERNAL_BFB_STATUS, "1.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsExternalUserInvalidStatusTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    cleanDatas.get(0).put(BulkParameters.EXTERNAL_BFB_STATUS, "3.0");
    cleanDatas.get(0).put(BulkParameters.BFB_BASE_PRICE, "13.0");
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "1.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      itemPickupPointListingL3Response);
    Assertions.assertTrue(b2BFields.isBuyable());
    Assertions.assertTrue(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
    Assertions.assertEquals(b2BFields.getPrice(),(Double)13.0);
  }

  @Test
  public void setBfbFieldsExternalUserInvalidStatusNullL3ResponseTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    cleanDatas.get(0).put(BulkParameters.EXTERNAL_BFB_STATUS, "3.0");
    cleanDatas.get(0).put(BulkParameters.BFB_MANAGED, "1.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0),
      null);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
    Assertions.assertTrue(b2BFields.isManaged());
  }

  @Test
  public void setBfbFieldsAmphiUserNullL3ResponseB2BTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response =
      new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setViewConfigs(
      List.of(new ProductLevel3ViewConfigResponse(Constant.B2B_CHANNEL, true, true)));
    itemPickupPointListingL3Response.setFreeSample(true);
    cleanDatas = generateExcelData(1);
    Map<String, Boolean> map = getPrivilegedMap();
    map.put(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    cleanDatas.get(0).put(BulkParameters.AMPHI_BFB_STATUS, "5.0");
    B2BFields b2BFields = bulkUpdateServiceUtil.setB2BFieldsFromExcel(map, cleanDatas.get(0), null);
    Assertions.assertFalse(b2BFields.isBuyable());
    Assertions.assertFalse(b2BFields.isDisplay());
  }

  @Test
  public void testAuthorizeUploadCampaignProductBulkUpdateTest() throws IOException {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    Row row = sheet.createRow(5);
    Cell cell = row.createCell(7);
    cell.setCellValue(BulkParameters.HARGA_REKOMENDASI);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"pricingCampaignRecommendationEnabled",true);
    bulkUpdateServiceUtil.authorizeUploadCampaignProductBulkUpdate(sheet, bulkProcess, new BulkUpdateQueue(), counter);
  }

  @Test
  public void testAuthorizeUploadCampaignProductBulkUpdateFalsePickupPointNameTest() throws IOException {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    Row row = sheet.createRow(5);
    Cell cell = row.createCell(3);
    cell.setCellValue(BulkParameters.PICKUP_POINT_NAME);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"pricingCampaignRecommendationEnabled",false);
    bulkUpdateServiceUtil.authorizeUploadCampaignProductBulkUpdate(sheet, bulkProcess, new BulkUpdateQueue(), counter);
  }

  @Test
  public void testAuthorizeUploadCampaignProductBulkUpdateFalseTest() throws IOException {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    Row row = sheet.createRow(5);
    Cell cell = row.createCell(7);
    cell.setCellValue(BulkParameters.REKOMENDASI);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"pricingCampaignRecommendationEnabled",false);
    bulkUpdateServiceUtil.authorizeUploadCampaignProductBulkUpdate(sheet, bulkProcess, new BulkUpdateQueue(), counter);
  }

  @Test
  public void testAuthorizeUploadCampaignProductBulkUpdateTrueTest() throws IOException {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Sheet1");
    Row row = sheet.createRow(5);
    Cell cell = row.createCell(7);
    cell.setCellValue(BulkParameters.REKOMENDASI);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"pricingCampaignRecommendationEnabled",true);
    bulkUpdateServiceUtil.authorizeUploadCampaignProductBulkUpdate(sheet, bulkProcess, new BulkUpdateQueue(), counter);
  }

  private Map<String, Boolean> getPrivilegedMap() {
    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRODUCT_TYPE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PICKUP_POINT, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_O2O, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_PRICE, true);
    privilegedMap.put(BulkParameters.IS_ONLY_EXTERNAL_USER, true);
    return privilegedMap;
  }

  @Test
  public void setScheduleRemovalForBulkProcessUpdateAndUpsertTest(){
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "updateScheduleRemovalEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "sellerPDPUrl",
      "seller.blilbi.com/");
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setParentProduct("TEC-15624-00001-00001");
    String notificationMessage = "Status telah diubah dan penjadwalan telah dihapus untuk produk Nama Produk. Lihat detail: <a href=seller.blilbi.com/TEC-15624-00001>click here</a>.";
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setScheduleRemovedForStatusUpdate(true);
    bulkUpdateServiceUtil.setScheduleRemovalForBulkProcessUpdateAndUpsert(Collections.singletonList(bulkProcessData),
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      PRODUCT_SKU, BulkParameters.PRODUCT_NAME);
    Assertions.assertEquals(notificationMessage, bulkProcessData.getIdentifier());
  }

  @Test
  public void setScheduleRemovalForBulkProcessUpdateAndUpsertEnglishTest(){
    profileResponse.getCompany().setInternationalFlag(true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "updateScheduleRemovalEnabled", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "sellerPDPUrl",
      "seller.blilbi.com/");
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setParentProduct("TEC-15624-00001-00001");
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData2.setParentProduct("TEC-15624-00002-00001");
    BulkProcessData bulkProcessData3 = new BulkProcessData();
    bulkProcessData3.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    String notificationMessage = "The status has been changed and schedule has been removed for "
      + "Nama Produk. View details: <a href=seller.blilbi.com/TEC-15624-00001>click here</a>.";
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setScheduleRemovedForStatusUpdate(true);
    bulkUpdateServiceUtil.setScheduleRemovalForBulkProcessUpdateAndUpsert(
        Arrays.asList(bulkProcessData, bulkProcessData2, bulkProcessData3),
        itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
        profileResponse.getCompany().isInternationalFlag(), PRODUCT_SKU, BulkParameters.PRODUCT_NAME);
    Assertions.assertEquals(notificationMessage, bulkProcessData.getIdentifier());
    Assertions.assertNull(bulkProcessData2.getIdentifier());
  }

  @Test
  public void setScheduleRemovalForBulkProcessUpdateAndUpsert_SwitchOffTest(){
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "updateScheduleRemovalEnabled", false);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setParentProduct("TEC-15624-00001-00001");
    itemsPriceStockImagesUpdateResponse.setScheduleRemovedForStatusUpdate(true);
    bulkUpdateServiceUtil.setScheduleRemovalForBulkProcessUpdateAndUpsert(Collections.singletonList(bulkProcessData),
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      BulkParameters.PRODUCT_SKU, BulkParameters.PRODUCT_NAME);
    verifyNoInteractions(bulkProcessService);
  }

  @Test
  public void setScheduleRemovalForBulkProcessUpdateAndUpsert_NoUpdateInScheduleTest(){
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "updateScheduleRemovalEnabled", true);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
      new ItemsPriceStockImagesUpdateResponse();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setParentProduct("TEC-15624-00001-00001");
    bulkUpdateServiceUtil.setScheduleRemovalForBulkProcessUpdateAndUpsert(Collections.singletonList(bulkProcessData),
      itemsPriceStockImagesUpdateResponse.getScheduleRemovedForStatusUpdate(),
      profileResponse.getCompany().isInternationalFlag(),
      BulkParameters.PRODUCT_SKU, BulkParameters.PRODUCT_NAME);
    verifyNoInteractions(bulkProcessService);
  }

  @Test
  public void extractProductSkuTest() {
    String productSku = BulkUpdateServiceUtil.extractProductSku("DR6-44662-00001");
    Assertions.assertEquals("DR6-44662-00001", productSku);
  }

  @Test
  public void extractProductSkuFromItemSkuTest() {
    String productSku = BulkUpdateServiceUtil.extractProductSku("DR6-44662-00001-00001");
    Assertions.assertEquals("DR6-44662-00001", productSku);
  }

  @Test
  public void extractProductSku_Invalid_Test() {
    String productSku = BulkUpdateServiceUtil.extractProductSku("446-62-00001");
    Assertions.assertEquals("446-62-00001", productSku);
  }

  @Test
  public void validateItemSkuByBusinessPartnerCodeTest() {
    Assertions.assertFalse(BulkUpdateServiceUtil.validateItemSkuByBusinessPartnerCode(PRODUCT_SKU, BULK_PROCESS_CODE));
    Assertions.assertFalse(BulkUpdateServiceUtil.validateItemSkuByBusinessPartnerCode(ITEM_SKU, BULK_PROCESS_CODE));
    Assertions.assertTrue(BulkUpdateServiceUtil.validateItemSkuByBusinessPartnerCode(ITEM_SKU, BUSINESS_PARTNER_CODE));
  }

  @Test
  public void validateBulkUpdateNewHeadersTest() {
    Assertions.assertFalse(BulkUpdateServiceUtil.validateBulkUpdateNewHeaders(headers));
    headers.put(1, BulkParameters.PICKUP_POINT_NAME_COLUMN_ID);
    headers.put(2, BulkParameters.PICKUP_POINT_HEADER);
    Assertions.assertFalse(BulkUpdateServiceUtil.validateBulkUpdateNewHeaders(headers));
    headers.put(3, BulkParameters.STOCK_HEADER);
    Assertions.assertTrue(BulkUpdateServiceUtil.validateBulkUpdateNewHeaders(headers));
  }

  @Test
  public void testIsMerchantEligibleForValidation_NullProfile() {
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(null,
      ACTION_TYPE_MERCHANT.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_NullCompany() {
    ProfileResponse profile = new ProfileResponse();
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile,
      ACTION_TYPE_MERCHANT.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_UnsupportedMerchantType() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.MERCHANT_TYPE_CM);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile,
      ACTION_TYPE_MERCHANT.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_SupportedMerchantTypeButFbbNotActivated() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(Constant.CC_MERCHANT);
    profile.setCompany(company);
    profile.setFbbActivated(false);
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile,
      ACTION_TYPE_MERCHANT.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_EligibleMerchant() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(ACTION_TYPE_MERCHANT);
    profile.setCompany(company);
    profile.setFbbActivated(true);

    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile,
      ACTION_TYPE_MERCHANT.concat(Constants.COMMA).concat(Constant.CC_MERCHANT));
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_EmptySupportedMerchants() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(ACTION_TYPE_MERCHANT);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile, "");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsMerchantEligibleForValidation_NullSupportedMerchants() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(ACTION_TYPE_MERCHANT);
    profile.setCompany(company);
    profile.setFbbActivated(true);
    boolean result = BulkUpdateServiceUtil.isMerchantEligibleForValidation(profile, null);
    Assertions.assertFalse(result);
  }

  @Test
  public void testToInventoryDetailInfoRequestDTO_WithValidRequest() {
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    InventoryDetailInfoRequestDTO result = BulkUpdateServiceUtil.toInventoryDetailInfoRequestDTO(request);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(ITEM_SKU, result.getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, result.getPickupPointCode());
  }

  @Test
  public void testValidateWareHouseStockAvailabilityForPPDeletion_WithPositiveStock() {
    InventoryDetailInfoResponseDTO inventoryResponse = new InventoryDetailInfoResponseDTO();
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setAvailableStock(10);
    inventoryResponse.setWebInventoryResponse(webInventoryResponse);
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventoryResponse, request, responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void testValidateWareHouseStockAvailabilityForPPDeletion_WithZeroStock() {
    InventoryDetailInfoResponseDTO inventoryResponse = new InventoryDetailInfoResponseDTO();
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setAvailableStock(0);
    inventoryResponse.setWebInventoryResponse(webInventoryResponse);
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventoryResponse, request, responses);
    Assertions.assertTrue(responses.isEmpty());
  }

  @Test
  public void testValidateWareHouseStockAvailabilityForPPDeletion_WithNegativeStock() {
    InventoryDetailInfoResponseDTO inventoryResponse = new InventoryDetailInfoResponseDTO();
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setAvailableStock(-5);
    inventoryResponse.setWebInventoryResponse(webInventoryResponse);
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventoryResponse, request, responses);
    Assertions.assertTrue(responses.isEmpty());
  }

  @Test
  public void testValidateWareHouseStockAvailabilityForPPDeletion_WithStock() {
    InventoryDetailInfoResponseDTO inventoryResponse = new InventoryDetailInfoResponseDTO();
    WebInventoryResponseDTO webInventoryResponse = new WebInventoryResponseDTO();
    webInventoryResponse.setAvailableStock(5);
    inventoryResponse.setWebInventoryResponse(webInventoryResponse);
    List<WarehouseInventoryResponseDTO> warehouseInventoryResponseList = new ArrayList<>();
    warehouseInventoryResponseList.add(WarehouseInventoryResponseDTO.builder().availableStock(10).build());
    inventoryResponse.setWarehouseInventoryResponseList(warehouseInventoryResponseList);
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventoryResponse, request, responses);
    Assertions.assertFalse(responses.isEmpty());
  }


  @Test
  public void testValidateWareHouseStockAvailabilityForPPDeletion_WithNullInventoryResponse() {
    InventoryDetailInfoResponseDTO inventoryResponse = new InventoryDetailInfoResponseDTO();
    DeleteOfflineItemRequest request = new DeleteOfflineItemRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE);
    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    BulkUpdateServiceUtil.validateWareHouseStockAvailabilityForPPDeletion(inventoryResponse, request, responses);
    Assertions.assertTrue(responses.isEmpty());
  }

  @Test
  public void testBulkProcessBasicInfoValidation_ValidRequest() {
    BulkBasicInfoRequest validRequest = new BulkBasicInfoRequest();
    validRequest.setBulkProcessType("VALID_TYPE");
    validRequest.setFileName("validFileName.xlsx");
    validRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Assertions.assertDoesNotThrow(() -> BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(validRequest));
  }

  @Test
  public void testBulkProcessBasicInfoValidation_InvalidBulkProcessType() {
    BulkBasicInfoRequest invalidRequest = new BulkBasicInfoRequest();
    invalidRequest.setBulkProcessType("");
    invalidRequest.setFileName("validFileName.xlsx");
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(invalidRequest));
    Assertions.assertTrue(exception.getMessage().contains(GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK));
  }

  @Test
  public void testBulkProcessBasicInfoValidation_InvalidFileName() {
    BulkBasicInfoRequest invalidRequest = new BulkBasicInfoRequest();
    invalidRequest.setBulkProcessType("VALID_TYPE");
    invalidRequest.setFileName("");
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(invalidRequest));
    Assertions.assertTrue(exception.getMessage().contains(GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK));
  }

  @Test
  public void testBulkProcessBasicInfoValidation_InvalidBusinessPartnerCode() {
    BulkBasicInfoRequest invalidRequest = new BulkBasicInfoRequest();
    invalidRequest.setBulkProcessType("VALID_TYPE");
    invalidRequest.setFileName(FILE_NAME);
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(invalidRequest));
    Assertions.assertTrue(
        exception.getMessage().contains(GenericErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
  }

  @Test
  public void testBulkProcessBasicInfoValidation_NullRequest() {
    BulkBasicInfoRequest nullRequest = new BulkBasicInfoRequest();
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> BulkUpdateServiceUtil.bulkProcessBasicInfoValidation(nullRequest));
    Assertions.assertTrue(exception.getMessage().contains(GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK));
  }

  @Test
  public void setFinalStatusForInputFailureTest(){
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setParentProduct("TEC-15624-00001-00001");
    bulkUpdateServiceUtil.setFinalStatusForInputFailure(bulkProcessData,
      bulkProcess,"error", 0, 0);
  }

  @Test
  void testCreateErrorHeaderStyle() {
    Workbook workbook = new XSSFWorkbook();
    CellStyle style = bulkUpdateServiceUtil.createErrorHeaderStyle(workbook);
    CellStyle msgStyle = bulkUpdateServiceUtil.createErrorMessageCellStyle(workbook);
    Assertions.assertNotNull(style);
    Assertions.assertNotNull(msgStyle);
  }

  @Test
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrderTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(-5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap,
        null, true, false,
        null, new HashSet<>(), profileResponse, null);
    Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrder_preOrderDTOFetchedTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(-5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1",BulkParameters.PO_QUOTA , "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap,
        null, true, false,
        null, new HashSet<>(), profileResponse, preOrderDTO);
    Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequest_PreOrderFetched_OMGSellerSwitchOnTest() throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(Date.from(LocalDateTime.now().plusDays(5)
        .atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse=new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true,ProfileFlagNames.BLIBLI_OMG,true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER, PICKUP_POINT_CODE,
            BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1", BulkParameters.SELLING_PRICE_HEADER,
            "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    itemPickupPointListingL3Response.setProductSku(PRODUCT_SKU);
    itemPickupPointListingL3Response.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingL3Response.setFbbActivated(false);
    itemPickupPointListingL3Response.setAvailableStockLevel2(10);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.setWebSyncStock(true);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
            PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE)).fbbActivated(true).build()))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID,PRODUCT_SKU)).thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data, privilegedMap, itemPickupPointListingL3Response, false, false,
              ImmutableMap.of(PICKUP_POINT_CODE_1, itemPickupPointListingL3Response),
              new HashSet<>(), profileResponse, preOrderDTO));
    } finally {
      Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
    }
  }

  @Test
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrder_switchOffTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(-5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap,
        null, true, false,
        null, new HashSet<>(), profileResponse, preOrderDTO);
    Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrder_nonOmgSellerTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(-5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, false));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap,
        null, true, false,
        null, new HashSet<>(), profileResponse, preOrderDTO);
    Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrder_dateCrossedTest()
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderDate(
        Date.from(LocalDateTime.now().minusDays(5).atZone(ZoneId.systemDefault()).toInstant()));
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0");
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest, productL5Data, privilegedMap,
        null, true, false,
        null, new HashSet<>(), profileResponse, preOrderDTO);
    Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @ParameterizedTest
  @ValueSource(strings =  {"omg-false","po-quota-string"})
  public void prepareUpdateL5SummaryRequest_addPickupPointPreOrder_dateCrossedWithPoQuotaColumn(String test)
      throws Exception {
    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    basicProductResponse = new BasicProductResponse();
    basicProductResponse.setPreOrder(preOrderDTO);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"preOrderQuotaFeatureSwitch",true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "faasFeatureSwitch", true);
    if(!test.equals("omg-false")) {
      profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true, ProfileFlagNames.BLIBLI_OMG, true));
    }
    profileResponse.setStoreId(STORE_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, String> productL5Data =
        new HashMap<>(Map.of(BulkParameters.BLIBLI_SKU, ITEM_SKU, BulkParameters.PICKUP_POINT_HEADER,
            PICKUP_POINT_CODE, BulkParameters.STOCK_HEADER, "1", BulkParameters.PRICE_HEADER, "1",
            BulkParameters.SELLING_PRICE_HEADER, "1", BulkParameters.CNC_STATUS_HEADER, "1.0"));
    if(test.equals("po-quota-string")){
      preOrderDTO.setPreOrderDate(
          Date.from(LocalDateTime.now().plusDays(5).atZone(ZoneId.systemDefault()).toInstant()));
      productL5Data.put(BulkParameters.PO_QUOTA,"abc");
    } else {
      preOrderDTO.setPreOrderDate(
          Date.from(LocalDateTime.now().minusDays(5).atZone(ZoneId.systemDefault()).toInstant()));
    }
    Map<String, Boolean> privilegedMap = new HashMap<>();
    setPrivilegedMap(privilegedMap);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    Mockito.when(businessPartnerRepository.filterBusinessPartnerPickupPointV2(0, 1,
        PickupPointFilterRequest.builder().codes(ImmutableSet.of(PICKUP_POINT_CODE))
            .fbbActivated(true).build())).thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest1 =
        new com.gda.mta.product.dto.ItemPickupPointRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest1));
    productItems.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productItems);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMerchantSku(MERCHANT_CODE);
    Mockito.when(xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    if (test.equals("po-quota-string")) {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
              productL5Data,
              privilegedMap,
              null,
              true,
              false,
              null,
              new HashSet<>(),
              profileResponse,
              preOrderDTO));
    } else {
      bulkUpdateServiceUtil.prepareUpdateL5SummaryRequest(productVariantUpdateRequest,
          productL5Data,
          privilegedMap,
          null,
          true,
          false,
          null,
          new HashSet<>(),
          profileResponse,
          preOrderDTO);
      Mockito.verify(xProductOutboundService).getItemBasicDetailsByItemSku(ITEM_SKU);
    }
    Assertions.assertTrue(productVariantUpdateRequest.getDeletePickupPoints().isEmpty());
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Success_AllRequiredAllRequiredHeadersPresent() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Selling Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    headers.add("Selling Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Failure_MissingRequiredAllRequiredHeaders() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Selling Price", "PRICE_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    // Missing "Selling Price"

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivilegesMap_Success_PartialPrivileges() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Failure_MultiplePrivilegesMissingAllRequiredHeaders() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");
    headerMap.put("Stock Reminder", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    // Missing "Regular Price", "Stock", "Stock Reminder"

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivilegesMap_Success_EmptyHeader() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Success_EmptyAllRequiredHeaders() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivilegesMap_Success_EmptyPrivileged() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Success_ExtraAllRequiredHeadersInExcel() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    headers.add("Extra Header 1");
    headers.add("Extra Header 2");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Failure_CaseSensitiveMismatch() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("price"); // lowercase
    headers.add("Regular Price");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Success_ComplexPrivilegeMapping() {
    // Setup - Multiple headers requiring same privilege
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Selling Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");
    headerMap.put("Stock Reminder", "STOCK_EDIT_PRIVILEGE");
    headerMap.put("Pickup Point", "PICKUP_POINT_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    headers.add("Selling Price");
    headers.add("Stock");
    headers.add("Stock Reminder");
    headers.add("Pickup Point");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);
    privilegedMap.put("PICKUP_POINT_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_Failure_ComplexPrivilegeMappingMissingAllRequiredHeaders() {
    // Setup - Multiple headers requiring same privilege, but some missing
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Regular Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Selling Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");
    headerMap.put("Stock Reminder", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Regular Price");
    // Missing "Selling Price", "Stock", "Stock Reminder"

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivilegesMap_Success_NullValuesInPrivileged() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", null);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", null);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivilegesMap_Success_AllPrivilegesFalse() {
    // Setup
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Stock");

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", false);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void authorizeUploadBulkUpdate_ValidateHeadersOnPrivileges_FeatureEnabled_ValidationPasses() throws Exception {
    // Setup - Feature flag enabled and validation passes
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateHeadersOnPrivileges", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);
    // Use the same Excel file as the working test case
    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);

    // Add all required headers to the Excel sheet based on PRIVILEGE_MAP
    Row headerRow = excelSheetData.getRow(0);
    if (headerRow != null) {
      // Define all required headers based on headerMap and PRIVILEGE_MAP
      String[] requiredHeaders = {
          "Harga (Rp)",               // Maps to PRIVILEGE_EDIT_PRICE
          "Harga Penjualan (Rp)",     // Maps to PRIVILEGE_EDIT_PRICE
          "Stok",                     // Maps to PRIVILEGE_EDIT_STOCK
          "Pengingat stok",           // Maps to PRIVILEGE_EDIT_STOCK
          "Tipe Penanganan",          // Maps to PRIVILEGE_EDIT_PRODUCT_TYPE
          "Toko/Gudang",              // Maps to PRIVILEGE_EDIT_PICKUP_POINT
          "Nama alamat pengambilan",  // Maps to PRIVILEGE_EDIT_PICKUP_POINT
          "Instore",        // Maps to PRIVILEGE_EDIT_O2O
          "Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)", // Maps to PRIVILEGE_EDIT_DISPLAY_BUYABLE
          "Status(0 = Offline, 1 = Online)", // Maps to PRIVILEGE_EDIT_DISPLAY_BUYABLE
          "Pengiriman(0 = Off, 1 = On)" // Maps to PRIVILEGE_EDIT_DISPLAY_BUYABLE
      };

      // Check which headers are missing and add them
      for (String requiredHeader : requiredHeaders) {
        boolean headerExists = false;
        for (int i = 0; i < headerRow.getLastCellNum(); i++) {
          Cell cell = headerRow.getCell(i);
          if (cell != null && requiredHeader.equalsIgnoreCase(cell.getStringCellValue())) {
            headerExists = true;
            break;
          }
        }

        if (!headerExists) {
          // Add the missing header to the next available column
          int nextColumn = headerRow.getLastCellNum();
          Cell newCell = headerRow.createCell(nextColumn);
          newCell.setCellValue(requiredHeader);
        }
      }
    }

    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();

    // Execute - This should pass because the Excel file contains all required headers for the privileges
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGED_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);

  }

  @Test
  public void authorizeUploadBulkUpdate_ValidateHeadersOnPrivileges_FeatureEnabled_ValidationFails() throws Exception {
    // Setup - Feature flag enabled but validation fails due to missing required headers
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateHeadersOnPrivileges", true);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);

    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);

    // Modify the Excel sheet to remove a required header that needs privilege validation
    Row headerRow = excelSheetData.getRow(0);
    if (headerRow != null) {
      // Remove a header that requires privilege validation (e.g., Price header)
      for (int i = 0; i < headerRow.getLastCellNum(); i++) {
        Cell cell = headerRow.getCell(i);
        if (cell != null && "Price".equalsIgnoreCase(cell.getStringCellValue())) {
          headerRow.removeCell(cell);
          break;
        }
      }
    }

    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();

    // Execute - This should fail because required headers are missing for privilege validation
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);

    // Verify - Should return false when feature is enabled and validation fails
    Assertions.assertFalse(result);
  }

  @Test
  public void authorizeUploadBulkUpdate_ValidateHeadersOnPrivileges_FeatureDisabled() throws Exception {
    // Setup - Feature flag disabled, validation should be skipped
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateHeadersOnPrivileges", false);
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "bulkUpdateHeaderValidation", true);

    Map<String, String> files = this.getFiles("BulkUpdate" + File.separator + "BulkUpdate.xlsx");
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String directoryPath = ProcessorUtils.BULK_UPDATE_DIR + BULK_PROCESS_CODE;
    ProcessorUtils.createDirectories(directoryPath);
    ProcessorUtils.createFile(directoryPath + File.separator + BULK_PROCESS_CODE + ProcessorUtils.FILETYPE_XLSX_EXCEL,
        excelFile);
    InputStream fileInputStream =
        BulkUpdateServiceUtil.getFileInputStream(BULK_PROCESS_CODE, "test.xlsx", ProcessorUtils.BULK_UPDATE_DIR);
    Sheet excelSheetData =
        POIUtil.getSheetForInputStream(fileInputStream, "test.xlsx".endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);

    // Modify the Excel sheet to remove a required header that would normally cause validation to fail
    Row headerRow = excelSheetData.getRow(0);
    if (headerRow != null) {
      // Remove a header that requires privilege validation (e.g., Price header)
      for (int i = 0; i < headerRow.getLastCellNum(); i++) {
        Cell cell = headerRow.getCell(i);
        if (cell != null && "Price".equalsIgnoreCase(cell.getStringCellValue())) {
          headerRow.removeCell(cell);
          break;
        }
      }
    }

    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    setBulkUpdateQueue(bulkUpdateQueue);
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();

    // Execute - This should pass because feature is disabled, so privilege validation is skipped
    boolean result =
        bulkUpdateServiceUtil.authorizeUploadBulkUpdate(PRIVILEGE_MAP, excelSheetData, bulkProcess, bulkUpdateQueue,
            counter, MerchantStatusType.PURE_DELIVERY, false, false);

    // Verify - Should return true when feature is disabled (validation skipped)
    Assertions.assertTrue(result);
  }

  // Test cases for PRIVILEGE_EDIT_DISPLAY_BUYABLE with external users
  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_DisplayBuyableAndCncStatus_Success() {
    // Setup - External user with both PRIVILEGE_EDIT_DISPLAY_BUYABLE and PRIVILEGE_EDIT_CNC_STATUS
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);
    headers.add(BulkParameters.DELIVERY_STATUS_HEADER);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_DisplayBuyableAndCncStatus_MissingDeliveryStatus() {
    // Setup - External user with both privileges but missing DELIVERY_STATUS_HEADER
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);
    // Missing DELIVERY_STATUS_HEADER

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_DisplayBuyableAndCncStatus_MissingExternalSkuStatus() {
    // Setup - External user with both privileges but missing EXTERNAL_SKU_STATUS_CNC_1P
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    // Missing EXTERNAL_SKU_STATUS_CNC_1P
    headers.add(BulkParameters.DELIVERY_STATUS_HEADER);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_DisplayBuyableOnly_Success() {
    // Setup - External user with only PRIVILEGE_EDIT_DISPLAY_BUYABLE
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_DisplayBuyableOnly_MissingHeader() {
    // Setup - External user with only PRIVILEGE_EDIT_DISPLAY_BUYABLE but missing required header
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    // Missing EXTERNAL_SKU_STATUS_CNC_1P

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  // Test cases for PRIVILEGE_EDIT_DISPLAY_BUYABLE with amphi users
  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_DisplayBuyableAndCncStatus_Success() {
    // Setup - Amphi user with both PRIVILEGE_EDIT_DISPLAY_BUYABLE and PRIVILEGE_EDIT_CNC_STATUS
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
    headers.add(BulkParameters.DELIVERY_STATUS_HEADER);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_DisplayBuyableAndCncStatus_MissingDeliveryStatus() {
    // Setup - Amphi user with both privileges but missing DELIVERY_STATUS_HEADER
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);
    // Missing DELIVERY_STATUS_HEADER

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_DisplayBuyableAndCncStatus_MissingAmphiSkuStatus() {
    // Setup - Amphi user with both privileges but missing AMPHI_SKU_STATUS_CNC_1P
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    // Missing AMPHI_SKU_STATUS_CNC_1P
    headers.add(BulkParameters.DELIVERY_STATUS_HEADER);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_DisplayBuyableOnly_Success() {
    // Setup - Amphi user with only PRIVILEGE_EDIT_DISPLAY_BUYABLE
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_DisplayBuyableOnly_MissingHeader() {
    // Setup - Amphi user with only PRIVILEGE_EDIT_DISPLAY_BUYABLE but missing required header
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    // Missing AMPHI_SKU_STATUS_CNC_1P

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", false);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertFalse(result);
  }

  // Test cases for edge cases and null values
  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_ExternalUser_NullIsOnlyExternalUser() {
    // Setup - External user with null isOnlyExternalUser
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", null);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify - Should behave like amphi user when isOnlyExternalUser is null
    Assertions.assertFalse(result); // Should fail because it's looking for AMPHI_SKU_STATUS_CNC_1P but we have EXTERNAL_SKU_STATUS_CNC_1P
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_AmphiUser_NullIsOnlyExternalUser() {
    // Setup - Amphi user with null isOnlyExternalUser
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.AMPHI_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.AMPHI_SKU_STATUS_CNC_1P);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", null);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify - Should behave like amphi user when isOnlyExternalUser is null
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_DisplayBuyableFalse() {
    // Setup - PRIVILEGE_EDIT_DISPLAY_BUYABLE is false, should not trigger special validation
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);
    headerMap.put(BulkParameters.DELIVERY_STATUS_HEADER, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);
    headers.add(BulkParameters.DELIVERY_STATUS_HEADER);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, false);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, true);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify - Should pass because PRIVILEGE_EDIT_DISPLAY_BUYABLE is false
    Assertions.assertTrue(result);
  }

  @Test
  public void testValidateAllRequiredHeadersPresentForPrivileges_MixedPrivileges_Success() {
    // Setup - Mix of regular privileges and special PRIVILEGE_EDIT_DISPLAY_BUYABLE
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    headers.add("Stock");
    headers.add(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P);

    Map<String, Boolean> privilegedMap = new HashMap<>();
    privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
    privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
    privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
    privilegedMap.put("isOnlyExternalUser", true);

    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, false);

    // Verify
    Assertions.assertTrue(result);
  }

  @ParameterizedTest
  @ValueSource(strings = {"poQuotaEnableTrue","poQuotaEnableFalse","poQuotaEnableFalsePrevFalse","poQuotaEnableTruePrevFalse"})
  public void testValidateAllRequiredHeadersPresentForPrivileges_MixedPrivileges_Failure(String test) {
    // Setup - Mix of regular privileges and special PRIVILEGE_EDIT_DISPLAY_BUYABLE with missing headers
    Map<String, String> headerMap = new HashMap<>();
    headerMap.put("Price", "PRICE_EDIT_PRIVILEGE");
    headerMap.put("Stock", "STOCK_EDIT_PRIVILEGE");
    headerMap.put(BulkParameters.EXTERNAL_SKU_STATUS_CNC_1P, BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE);

    Set<String> headers = new HashSet<>();
    headers.add("Price");
    // Missing "Stock" and EXTERNAL_SKU_STATUS_CNC_1P

    Map<String, Boolean> privilegedMap = new HashMap<>();
    boolean poQuotaEnable;
    switch (test) {
      case "poQuotaEnableTrue" -> {
        poQuotaEnable = true;
        privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, true);
      }
      case "poQuotaEnableTruePrevFalse" -> {
        poQuotaEnable = true;
        privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
      }
      case "poQuotaEnableFalsePrevFalse" -> {
        poQuotaEnable = false;
        privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_STOCK, false);
      }
      default -> {
        poQuotaEnable = false;
        privilegedMap.put("PRICE_EDIT_PRIVILEGE", true);
        privilegedMap.put("STOCK_EDIT_PRIVILEGE", true);
        privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_DISPLAY_BUYABLE, true);
        privilegedMap.put(BulkParameters.PRIVILEGE_EDIT_CNC_STATUS, false);
        privilegedMap.put("isOnlyExternalUser", true);
      }
    }
    // Execute
    boolean result = bulkUpdateServiceUtil.validateAllRequiredHeadersPresentForPrivileges(headerMap, headers, privilegedMap, poQuotaEnable);

    // Verify
    switch (test) {
      case "poQuotaEnableTrue", "poQuotaEnableTruePrevFalse", "poQuotaEnableFalsePrevFalse" ->
          Assertions.assertTrue(result);
      default -> Assertions.assertFalse(result);
    }
  }

  @Test
  void testValidateExcelFileForBulkProductsArchive_withRealExcel() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"validateBulkArchiveHeadersEnabled",true);
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "BulkArchiveTemplate.xlsx");
    XSSFWorkbook workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    BulkProcess bulkProcess = new BulkProcess();
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkProductsArchive(sheet, bulkProcess, bulkUpdateQueue, counter);
    Assertions.assertTrue(result);
  }

  @Test
  void testValidateExcelFileForBulkProductsArchive_removeSecondHeader_returnsFalse() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateBulkArchiveHeadersEnabled", true);
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "BulkArchiveTemplate.xlsx");
    XSSFWorkbook workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    sheet.getRow(0).removeCell(sheet.getRow(0).getCell(1));
    BulkProcess bulkProcess = new BulkProcess();
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkProductsArchive(
        sheet, bulkProcess, bulkUpdateQueue, counter);
    Assertions.assertFalse(result);
  }

  @Test
  void testValidateExcelFileForBulkProductsArchive_removeFirstHeader_returnsFalse() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil, "validateBulkArchiveHeadersEnabled", true);
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "BulkArchiveTemplate.xlsx");
    XSSFWorkbook workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    sheet.getRow(0).removeCell(sheet.getRow(0).getCell(0));
    BulkProcess bulkProcess = new BulkProcess();
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkProductsArchive(
        sheet, bulkProcess, bulkUpdateQueue, counter);
    Assertions.assertFalse(result, "Validation should fail if PARENT_PRODUCT_NAME header is removed");
  }

  @Test
  void testValidateExcelFileForBulkProductsArchive_withRealExcel1() throws Exception {
    ReflectionTestUtils.setField(bulkUpdateServiceUtil,"validateBulkArchiveHeadersEnabled",true);
    InputStream is = Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "BulkArchiveTemplate.xlsx");
    XSSFWorkbook workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    sheet.getRow(0).createCell(3).setCellValue("NEW_HEADER");
    BulkProcess bulkProcess = new BulkProcess();
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    boolean result = bulkUpdateServiceUtil.validateExcelFileForBulkProductsArchive(sheet, bulkProcess, bulkUpdateQueue, counter);
    Assertions.assertFalse(result);
  }

  @Test
  void testAuthorizeUploadBulkUpdateEAN_EmptyHeaders() throws Exception {
    // Test case where headers are empty - should return false and call updateBulkStatusAborted
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BULK123");
    
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();

    boolean result = bulkUpdateServiceUtil.authorizeUploadBulkUpdateEAN(bulkProcess, counter,
        bulkUpdateQueue.getStoreId(), new HashMap<>(), bulkUpdateQueue.getBulkProcessCode(),
        bulkUpdateQueue.getBusinessPartnerCode());
    
    Assertions.assertFalse(result, "Should return false when headers are empty");
    verify(bulkProcessService).saveOperation(bulkProcess);
  }

  @Test
  void testAuthorizeUploadBulkUpdateEAN_AllRequiredHeadersPresent() throws Exception {
    // Test case where all required headers are present - should return true
    Map<Integer, String> headerRow = new HashMap<>();
    
    // Create headers with all required EAN headers
    headerRow.put(0,BulkParameters.BLIBLI_PRODUCT_SKU);
    headerRow.put(1,BulkParameters.PARENT_PRODUCT_NAME);
    headerRow.put(2,BulkParameters.BLIBLI_SKU);
    headerRow.put(3,BulkParameters.NAMA_PRODUK);
    headerRow.put(4,BulkParameters.EAN_OR_UPC);
    
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BULK123");
    
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    
    boolean result = bulkUpdateServiceUtil.authorizeUploadBulkUpdateEAN(bulkProcess, counter,
        bulkUpdateQueue.getStoreId(), headerRow, bulkUpdateQueue.getBulkProcessCode(),
        bulkUpdateQueue.getBusinessPartnerCode());
    
    Assertions.assertTrue(result, "Should return true when all required headers are present");
    verifyNoInteractions(bulkProcessService);
  }

  @Test
  void testAuthorizeUploadBulkUpdateEAN_MissingRequiredHeaders() throws Exception {
    // Test case where some required headers are missing - should return false
    Map<Integer, String> headerRow = new HashMap<>();

    // Create headers but missing EAN_OR_UPC
    headerRow.put(0,BulkParameters.BLIBLI_PRODUCT_SKU);
    headerRow.put(1,BulkParameters.PARENT_PRODUCT_NAME);
    headerRow.put(2,BulkParameters.BLIBLI_SKU);
    headerRow.put(3,BulkParameters.NAMA_PRODUK);
    // Missing BulkParameters.EAN_OR_UPC
    
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BULK123");
    
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setStoreId("10001");
    bulkUpdateQueue.setBulkProcessCode("BULK123");
    bulkUpdateQueue.setBusinessPartnerCode("BP123");
    
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    
    boolean result = bulkUpdateServiceUtil.authorizeUploadBulkUpdateEAN(bulkProcess, counter,
        bulkUpdateQueue.getStoreId(), headerRow, bulkUpdateQueue.getBulkProcessCode(),
        bulkUpdateQueue.getBusinessPartnerCode());
    
    Assertions.assertFalse(result, "Should return false when required headers are missing");
    verifyNoInteractions(bulkProcessService);
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_EmptyList() throws Exception {
    // Test case where cleanDataList is empty - should return empty list
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertTrue(result.isEmpty(), "Should return empty list when cleanDataList is empty");
    Assertions.assertTrue(successList.isEmpty(), "Success list should be empty");
    Assertions.assertTrue(failureList.isEmpty(), "Failure list should be empty");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_ValidData() throws Exception {
    // Test case where all validations pass - data should be added to successList
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> validData = new LinkedHashMap<>();
    validData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    validData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "BP123-PRD-001");
    validData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(validData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertTrue(result.isEmpty(), "Should return empty error list when validation passes");
    Assertions.assertEquals(1, successList.size(), "Should have one item in success list");
    Assertions.assertEquals(validData, successList.get(0), "Success list should contain valid data");
    Assertions.assertTrue(failureList.isEmpty(), "Failure list should be empty");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_MissingBlibliSku() throws Exception {
    // Test case where BLIBLI_SKU is empty - should fail and add to failureList
    // Note: validateMandatoryColumn checks if key exists AND is empty
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> invalidData = new LinkedHashMap<>();
    invalidData.put(BulkParameters.BLIBLI_SKU, ""); // Empty BLIBLI_SKU
    invalidData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "BP123-PRD-001");
    invalidData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(invalidData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertEquals(1, result.size(), "Should have one error DTO");
    Assertions.assertTrue(successList.isEmpty(), "Success list should be empty");
    Assertions.assertEquals(1, failureList.size(), "Should have one item in failure list");
    Assertions.assertEquals(invalidData, failureList.get(0), "Failure list should contain invalid data");
    Assertions.assertTrue(result.get(0).getReason().contains("Blibli-SKU tidak boleh kosong"),
        "Error message should contain BLIBLI_SKU_BLANK");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_MissingBlibliProductSku() throws Exception {
    // Test case where BLIBLI_PRODUCT_SKU is empty - should fail and add to failureList
    // Note: validateMandatoryColumn checks if key exists AND is empty
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> invalidData = new LinkedHashMap<>();
    invalidData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    invalidData.put(BulkParameters.BLIBLI_PRODUCT_SKU, ""); // Empty BLIBLI_PRODUCT_SKU
    invalidData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(invalidData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertEquals(1, result.size(), "Should have one error DTO");
    Assertions.assertTrue(successList.isEmpty(), "Success list should be empty");
    Assertions.assertEquals(1, failureList.size(), "Should have one item in failure list");
    Assertions.assertEquals(invalidData, failureList.get(0), "Failure list should contain invalid data");
    Assertions.assertTrue(result.get(0).getReason().contains("Blibli Product SKU tidak boleh kosong"),
        "Error message should contain BLIBLI_PRODUCT_SKU_BLANK");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_InvalidBlibliSkuNotPartOfSeller() throws Exception {
    // Test case where BLIBLI_SKU doesn't start with businessPartnerCode - should fail
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> invalidData = new LinkedHashMap<>();
    invalidData.put(BulkParameters.BLIBLI_SKU, "OTHER-00117-00001"); // Doesn't start with BP123
    invalidData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "BP123-PRD-001");
    invalidData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(invalidData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertEquals(1, result.size(), "Should have one error DTO");
    Assertions.assertTrue(successList.isEmpty(), "Success list should be empty");
    Assertions.assertEquals(1, failureList.size(), "Should have one item in failure list");
    Assertions.assertTrue(result.get(0).getReason().contains("BP123"),
        "Error message should contain business partner code");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_InvalidBlibliProductSkuNotPartOfSeller() throws Exception {
    // Test case where BLIBLI_PRODUCT_SKU doesn't start with businessPartnerCode - should fail
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> invalidData = new LinkedHashMap<>();
    invalidData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    invalidData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "OTHER-PRD-001"); // Doesn't start with BP123
    invalidData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(invalidData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertEquals(1, result.size(), "Should have one error DTO");
    Assertions.assertTrue(successList.isEmpty(), "Success list should be empty");
    Assertions.assertEquals(1, failureList.size(), "Should have one item in failure list");
    Assertions.assertTrue(result.get(0).getReason().contains("BP123"),
        "Error message should contain business partner code");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_EmptyBusinessPartnerCode() throws Exception {
    // Test case where businessPartnerCode is empty - should skip seller validation
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    Map<String, String> validData = new LinkedHashMap<>();
    validData.put(BulkParameters.BLIBLI_SKU, "ANY-00117-00001");
    validData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "ANY-PRD-001");
    validData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    cleanDataList.add(validData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = ""; // Empty business partner code

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertTrue(result.isEmpty(), "Should return empty error list when validation passes");
    Assertions.assertEquals(1, successList.size(), "Should have one item in success list");
    Assertions.assertTrue(failureList.isEmpty(), "Failure list should be empty");
  }

  @Test
  void testValidateExcelDataBulkUpdateEANProduct_MixedSuccessAndFailure() throws Exception {
    // Test case with multiple items - some valid, some invalid
    List<Map<String, String>> cleanDataList = new ArrayList<>();
    
    // Valid data
    Map<String, String> validData = new LinkedHashMap<>();
    validData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    validData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "BP123-PRD-001");
    validData.put(BulkParameters.NAMA_PRODUK, "Valid Product");
    cleanDataList.add(validData);
    
    // Invalid data - empty BLIBLI_SKU (validateMandatoryColumn checks if key exists AND is empty)
    Map<String, String> invalidData = new LinkedHashMap<>();
    invalidData.put(BulkParameters.BLIBLI_SKU, ""); // Empty BLIBLI_SKU
    invalidData.put(BulkParameters.BLIBLI_PRODUCT_SKU, "BP123-PRD-002");
    invalidData.put(BulkParameters.NAMA_PRODUK, "Invalid Product");
    cleanDataList.add(invalidData);

    List<Map<String, String>> successList = new ArrayList<>();
    List<Map<String, String>> failureList = new ArrayList<>();
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    String businessPartnerCode = "BP123";

    List<BulkUpdateErrorDTO> result = bulkUpdateServiceUtil.validateExcelDataBulkUpdateEANProduct(
        cleanDataList, successList, failureList, counter, businessPartnerCode);

    Assertions.assertEquals(1, result.size(), "Should have one error DTO");
    Assertions.assertEquals(1, successList.size(), "Should have one item in success list");
    Assertions.assertEquals(1, failureList.size(), "Should have one item in failure list");
    Assertions.assertEquals(validData, successList.get(0), "Success list should contain valid data");
    Assertions.assertEquals(invalidData, failureList.get(0), "Failure list should contain invalid data");
  }

  @Test
  void testUpdateErrorDTOListForL4EANUpdate_SuccessFlagTrue() throws Exception {
    // Test case for updateErrorDTOListForL4EANUpdate when successFlag is true
    // Should add cleanData to successDataList
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    cleanData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    
    StringBuilder tempErrorMessage = new StringBuilder();
    boolean successFlag = true;
    List<Map<String, String>> successDataList = new ArrayList<>();
    List<Map<String, String>> failureDataList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    int initialInputErrorCount = bulkUpdateErrorCounter.getInputErrorCounter();
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "updateErrorDTOListForL4EANUpdate",
        StringBuilder.class,
        boolean.class,
        Map.class,
        List.class,
        List.class,
        List.class,
        BulkUpdateErrorCounter.class);
    method.setAccessible(true);
    method.invoke(null, tempErrorMessage, successFlag, cleanData, successDataList, 
        failureDataList, bulkUpdateErrorDTOList, bulkUpdateErrorCounter);
    
    Assertions.assertEquals(1, successDataList.size(), "Should have one item in success list");
    Assertions.assertEquals(cleanData, successDataList.get(0), "Success list should contain cleanData");
    Assertions.assertTrue(failureDataList.isEmpty(), "Failure list should be empty");
    Assertions.assertTrue(bulkUpdateErrorDTOList.isEmpty(), "Error DTO list should be empty");
    Assertions.assertEquals(initialInputErrorCount, bulkUpdateErrorCounter.getInputErrorCounter(),
        "Input error counter should not be incremented when successFlag is true");
  }

  @Test
  void testUpdateErrorDTOListForL4EANUpdate_SuccessFlagFalse() throws Exception {
    // Test case for updateErrorDTOListForL4EANUpdate when successFlag is false
    // Should add cleanData to failureDataList, increment counter, and create error DTO
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    cleanData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    
    StringBuilder tempErrorMessage = new StringBuilder();
    tempErrorMessage.append("Error message 1").append(BulkUpdateServiceUtil.AND_SYMBOL);
    tempErrorMessage.append("Error message 2").append(BulkUpdateServiceUtil.AND_SYMBOL);
    
    boolean successFlag = false;
    List<Map<String, String>> successDataList = new ArrayList<>();
    List<Map<String, String>> failureDataList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    int initialInputErrorCount = bulkUpdateErrorCounter.getInputErrorCounter();
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "updateErrorDTOListForL4EANUpdate",
        StringBuilder.class,
        boolean.class,
        Map.class,
        List.class,
        List.class,
        List.class,
        BulkUpdateErrorCounter.class);
    method.setAccessible(true);
    method.invoke(null, tempErrorMessage, successFlag, cleanData, successDataList, 
        failureDataList, bulkUpdateErrorDTOList, bulkUpdateErrorCounter);
    
    Assertions.assertTrue(successDataList.isEmpty(), "Success list should be empty");
    Assertions.assertEquals(1, failureDataList.size(), "Should have one item in failure list");
    Assertions.assertEquals(cleanData, failureDataList.get(0), "Failure list should contain cleanData");
    Assertions.assertEquals(1, bulkUpdateErrorDTOList.size(), "Should have one error DTO");
    Assertions.assertEquals(initialInputErrorCount + 1, bulkUpdateErrorCounter.getInputErrorCounter(),
        "Input error counter should be incremented when successFlag is false");
    
    BulkUpdateErrorDTO errorDTO = bulkUpdateErrorDTOList.get(0);
    Assertions.assertEquals(cleanData.get(BulkParameters.NAMA_PRODUK), errorDTO.getProductName(),
        "Error DTO product name should match NAMA_PRODUK");
    Assertions.assertEquals(cleanData.get(BulkParameters.BLIBLI_SKU), errorDTO.getProductSku(),
        "Error DTO product SKU should match BLIBLI_SKU");
    // Error message should have last AND_SYMBOL removed
    String expectedErrorMessage = "Error message 1" + BulkUpdateServiceUtil.AND_SYMBOL + "Error message 2";
    Assertions.assertEquals(expectedErrorMessage, errorDTO.getReason(),
        "Error message should have last AND_SYMBOL removed");
  }

  @Test
  void testUpdateErrorDTOListForL4EANUpdate_SuccessFlagFalse_EmptyErrorMessage() throws Exception {
    // Test case for updateErrorDTOListForL4EANUpdate when successFlag is false and error message is empty
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    cleanData.put(BulkParameters.NAMA_PRODUK, "Test Product");
    
    StringBuilder tempErrorMessage = new StringBuilder(); // Empty error message
    
    boolean successFlag = false;
    List<Map<String, String>> successDataList = new ArrayList<>();
    List<Map<String, String>> failureDataList = new ArrayList<>();
    List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList = new ArrayList<>();
    BulkUpdateErrorCounter bulkUpdateErrorCounter = new BulkUpdateErrorCounter();
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "updateErrorDTOListForL4EANUpdate",
        StringBuilder.class,
        boolean.class,
        Map.class,
        List.class,
        List.class,
        List.class,
        BulkUpdateErrorCounter.class);
    method.setAccessible(true);
    method.invoke(null, tempErrorMessage, successFlag, cleanData, successDataList, 
        failureDataList, bulkUpdateErrorDTOList, bulkUpdateErrorCounter);
    
    Assertions.assertEquals(1, failureDataList.size(), "Should have one item in failure list");
    Assertions.assertEquals(1, bulkUpdateErrorDTOList.size(), "Should have one error DTO");
    Assertions.assertEquals("", bulkUpdateErrorDTOList.get(0).getReason(),
        "Error message should be empty when tempErrorMessage is empty");
  }

  @Test
  void testSetEanUpdateFinalDataStatus_WithSuccessAndError() throws Exception {
    // Test case where some items succeed and some fail
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    
    // Create BulkProcessData with bulkRequestData containing BLIBLI_SKU
    BulkProcessData successData = new BulkProcessData();
    Map<String, String> successRowData = new LinkedHashMap<>();
    successRowData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    String successRowDataJson = mapper.writeValueAsString(successRowData);
    successData.setBulkRequestData(successRowDataJson);
    bulkProcessDataList.add(successData);
    
    BulkProcessData errorData = new BulkProcessData();
    Map<String, String> errorRowData = new LinkedHashMap<>();
    errorRowData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00002");
    String errorRowDataJson = mapper.writeValueAsString(errorRowData);
    errorData.setBulkRequestData(errorRowDataJson);
    bulkProcessDataList.add(errorData);
    
    // Create error DTO for the second item
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("BP123-00117-00002");
    errorDTO.setReason("Validation error");
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    listBulkUpdateErrorDTO.add(errorDTO);
    
    // Create success DTO for the first item
    BulkUpdateSuccessDTO successDTO = BulkUpdateSuccessDTO.builder()
        .productSku("BP123-00117-00001")
        .productName("Test Product")
        .build();
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    bulkUpdateSuccessDTOList.add(successDTO);
    
    // Mock objectMapper.readValue for reading bulkRequestData
    when(objectMapper.readValue(eq(successRowDataJson), any(TypeReference.class))).thenReturn(successRowData);
    when(objectMapper.readValue(eq(errorRowDataJson), any(TypeReference.class))).thenReturn(errorRowData);
    
    // Mock objectMapper.writeValueAsString for writing DTOs
    String successDTOJson = mapper.writeValueAsString(successDTO);
    String errorDTOJson = mapper.writeValueAsString(errorDTO);
    when(objectMapper.writeValueAsString(eq(successDTO))).thenReturn(successDTOJson);
    when(objectMapper.writeValueAsString(eq(errorDTO))).thenReturn(errorDTOJson);
    
    bulkUpdateServiceUtil.setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, bulkUpdateSuccessDTOList);
    
    // Verify success data
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, successData.getStatus(),
        "Success data should have STATUS_SUCCESS");
    Assertions.assertNotNull(successData.getNotes(), "Success data should have notes");
    Assertions.assertNotNull(successData.getEndDate(), "Success data should have endDate");
    Assertions.assertEquals(successDTOJson, successData.getNotes(), "Notes should contain serialized success DTO");
    
    // Verify error data
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, errorData.getStatus(),
        "Error data should have STATUS_FAIL");
    Assertions.assertNotNull(errorData.getErrorMessage(), "Error data should have errorMessage");
    Assertions.assertEquals(1, errorData.getInputErrorCount(), "Error data should have inputErrorCount = 1");
    Assertions.assertNull(errorData.getSystemErrorCount(), "Error data should have systemErrorCount = null (not set for input errors)");
    Assertions.assertNotNull(errorData.getEndDate(), "Error data should have endDate");
    Assertions.assertEquals(errorDTOJson, errorData.getErrorMessage(), "ErrorMessage should contain serialized error DTO");
  }

  @Test
  void testSetEanUpdateFinalDataStatus_WithInternalError() throws Exception {
    // Test case where error has INTERNAL_ERROR - should set systemErrorCount
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    
    BulkProcessData errorData = new BulkProcessData();
    Map<String, String> errorRowData = new LinkedHashMap<>();
    errorRowData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    String errorRowDataJson = mapper.writeValueAsString(errorRowData);
    errorData.setBulkRequestData(errorRowDataJson);
    bulkProcessDataList.add(errorData);
    
    // Create error DTO with INTERNAL_ERROR
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("BP123-00117-00001");
    errorDTO.setReason(BulkUpdateServiceUtil.INTERNAL_ERROR);
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    listBulkUpdateErrorDTO.add(errorDTO);
    
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    
    // Mock objectMapper.readValue for reading bulkRequestData
    when(objectMapper.readValue(eq(errorRowDataJson), any(TypeReference.class))).thenReturn(errorRowData);
    
    // Mock objectMapper.writeValueAsString for writing error DTO
    String errorDTOJson = mapper.writeValueAsString(errorDTO);
    when(objectMapper.writeValueAsString(eq(errorDTO))).thenReturn(errorDTOJson);
    
    bulkUpdateServiceUtil.setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, bulkUpdateSuccessDTOList);
    
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, errorData.getStatus(),
        "Error data should have STATUS_FAIL");
    Assertions.assertNull(errorData.getInputErrorCount(), "Error data should have inputErrorCount = null (not set for internal errors)");
    Assertions.assertEquals(1, errorData.getSystemErrorCount(), "Error data should have systemErrorCount = 1");
    Assertions.assertEquals(errorDTOJson, errorData.getErrorMessage(), "ErrorMessage should contain serialized error DTO");
  }

  @Test
  void testSetEanUpdateFinalDataStatus_AllSuccess() throws Exception {
    // Test case where all items succeed
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    
    BulkProcessData successData1 = new BulkProcessData();
    Map<String, String> rowData1 = new LinkedHashMap<>();
    rowData1.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    String rowData1Json = mapper.writeValueAsString(rowData1);
    successData1.setBulkRequestData(rowData1Json);
    bulkProcessDataList.add(successData1);
    
    BulkProcessData successData2 = new BulkProcessData();
    Map<String, String> rowData2 = new LinkedHashMap<>();
    rowData2.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00002");
    String rowData2Json = mapper.writeValueAsString(rowData2);
    successData2.setBulkRequestData(rowData2Json);
    bulkProcessDataList.add(successData2);
    
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    
    BulkUpdateSuccessDTO successDTO1 = BulkUpdateSuccessDTO.builder()
        .productSku("BP123-00117-00001")
        .productName("Product 1")
        .build();
    BulkUpdateSuccessDTO successDTO2 = BulkUpdateSuccessDTO.builder()
        .productSku("BP123-00117-00002")
        .productName("Product 2")
        .build();
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    bulkUpdateSuccessDTOList.add(successDTO1);
    bulkUpdateSuccessDTOList.add(successDTO2);
    
    // Mock objectMapper.readValue for reading bulkRequestData
    when(objectMapper.readValue(eq(rowData1Json), any(TypeReference.class))).thenReturn(rowData1);
    when(objectMapper.readValue(eq(rowData2Json), any(TypeReference.class))).thenReturn(rowData2);
    
    // Mock objectMapper.writeValueAsString for writing success DTOs
    String successDTO1Json = mapper.writeValueAsString(successDTO1);
    String successDTO2Json = mapper.writeValueAsString(successDTO2);
    when(objectMapper.writeValueAsString(eq(successDTO1))).thenReturn(successDTO1Json);
    when(objectMapper.writeValueAsString(eq(successDTO2))).thenReturn(successDTO2Json);
    
    bulkUpdateServiceUtil.setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, bulkUpdateSuccessDTOList);
    
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, successData1.getStatus(),
        "First data should have STATUS_SUCCESS");
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, successData2.getStatus(),
        "Second data should have STATUS_SUCCESS");
    Assertions.assertNotNull(successData1.getNotes(), "First data should have notes");
    Assertions.assertNotNull(successData2.getNotes(), "Second data should have notes");
  }

  @Test
  void testSetEanUpdateFinalDataStatus_AllErrors() throws Exception {
    // Test case where all items fail
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    
    BulkProcessData errorData1 = new BulkProcessData();
    Map<String, String> rowData1 = new LinkedHashMap<>();
    rowData1.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001");
    String rowData1Json = mapper.writeValueAsString(rowData1);
    errorData1.setBulkRequestData(rowData1Json);
    bulkProcessDataList.add(errorData1);
    
    BulkProcessData errorData2 = new BulkProcessData();
    Map<String, String> rowData2 = new LinkedHashMap<>();
    rowData2.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00002");
    String rowData2Json = mapper.writeValueAsString(rowData2);
    errorData2.setBulkRequestData(rowData2Json);
    bulkProcessDataList.add(errorData2);
    
    BulkUpdateErrorDTO errorDTO1 = new BulkUpdateErrorDTO();
    errorDTO1.setProductSku("BP123-00117-00001");
    errorDTO1.setReason("Error 1");
    BulkUpdateErrorDTO errorDTO2 = new BulkUpdateErrorDTO();
    errorDTO2.setProductSku("BP123-00117-00002");
    errorDTO2.setReason("Error 2");
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    listBulkUpdateErrorDTO.add(errorDTO1);
    listBulkUpdateErrorDTO.add(errorDTO2);
    
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    
    // Mock objectMapper.readValue for reading bulkRequestData
    when(objectMapper.readValue(eq(rowData1Json), any(TypeReference.class))).thenReturn(rowData1);
    when(objectMapper.readValue(eq(rowData2Json), any(TypeReference.class))).thenReturn(rowData2);
    
    // Mock objectMapper.writeValueAsString for writing error DTOs
    String errorDTO1Json = mapper.writeValueAsString(errorDTO1);
    String errorDTO2Json = mapper.writeValueAsString(errorDTO2);
    when(objectMapper.writeValueAsString(eq(errorDTO1))).thenReturn(errorDTO1Json);
    when(objectMapper.writeValueAsString(eq(errorDTO2))).thenReturn(errorDTO2Json);
    
    bulkUpdateServiceUtil.setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, bulkUpdateSuccessDTOList);
    
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, errorData1.getStatus(),
        "First data should have STATUS_FAIL");
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, errorData2.getStatus(),
        "Second data should have STATUS_FAIL");
    Assertions.assertEquals(1, errorData1.getInputErrorCount(), "First data should have inputErrorCount = 1");
    Assertions.assertEquals(1, errorData2.getInputErrorCount(), "Second data should have inputErrorCount = 1");
  }

  @Test
  void testSetEanUpdateFinalDataStatus_ItemNotInMaps() throws Exception {
    // Test case where item is not in error map and not in success map
    // Should still set status to SUCCESS (else branch at line 3520-3523)
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();
    
    BulkProcessData data = new BulkProcessData();
    Map<String, String> rowData = new LinkedHashMap<>();
    rowData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00099"); // SKU not in any map
    String rowDataJson = mapper.writeValueAsString(rowData);
    data.setBulkRequestData(rowDataJson);
    bulkProcessDataList.add(data);
    
    // Create maps with different SKUs
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("BP123-00117-00001"); // Different SKU
    List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
    listBulkUpdateErrorDTO.add(errorDTO);
    
    BulkUpdateSuccessDTO successDTO = BulkUpdateSuccessDTO.builder()
        .productSku("BP123-00117-00002") // Different SKU
        .build();
    List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOList = new ArrayList<>();
    bulkUpdateSuccessDTOList.add(successDTO);
    
    // Mock objectMapper.readValue for reading bulkRequestData
    when(objectMapper.readValue(eq(rowDataJson), any(TypeReference.class))).thenReturn(rowData);
    
    // Mock objectMapper.writeValueAsString for writing null (when itemSkuSuccessMap.get() returns null)
    when(objectMapper.writeValueAsString(isNull())).thenReturn("null");
    
    bulkUpdateServiceUtil.setEanUpdateFinalDataStatus(bulkProcessDataList, listBulkUpdateErrorDTO, bulkUpdateSuccessDTOList);
    
    // When not in error map, it goes to else branch and tries to get from success map
    // If not in success map, itemSkuSuccessMap.get() returns null, which gets serialized
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, data.getStatus(),
        "Data should have STATUS_SUCCESS when not in error map (else branch)");
    Assertions.assertNotNull(data.getEndDate(), "Data should have endDate");
    // Notes will contain "null" as JSON string since itemSkuSuccessMap.get() returns null
    Assertions.assertNotNull(data.getNotes(), "Data should have notes (even if null)");
  }

  @Test
  void testValidateMandatoryColumn_KeyExistsAndEmpty() throws Exception {
    // Test case where key exists but value is empty - should fail validation
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, ""); // Empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertFalse(result, "Should return false when mandatory column is empty");
    Assertions.assertEquals(1, counter.getBlibliSkuCounter(), "Blibli SKU counter should be incremented");
    Assertions.assertTrue(tempErrorMessage.toString().contains(BulkUpdateServiceUtil.BLIBLI_SKU_BLANK),
        "Error message should contain BLIBLI_SKU_BLANK");
    Assertions.assertTrue(tempErrorMessage.toString().endsWith(BulkUpdateServiceUtil.AND_SYMBOL),
        "Error message should end with AND_SYMBOL");
  }

  @Test
  void testValidateMandatoryColumn_KeyExistsAndNotEmpty() throws Exception {
    // Test case where key exists and value is not empty - should pass validation
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, "BP123-00117-00001"); // Non-empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertTrue(result, "Should return true when mandatory column is not empty");
    Assertions.assertEquals(0, counter.getBlibliSkuCounter(), "Blibli SKU counter should not be incremented");
    Assertions.assertTrue(tempErrorMessage.toString().isEmpty(),
        "Error message should be empty when validation passes");
  }

  @Test
  void testValidateMandatoryColumn_KeyDoesNotExist() throws Exception {
    // Test case where key does not exist - should pass validation (only checks if key exists AND is empty)
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    // Key not added to cleanData
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertTrue(result, "Should return true when key does not exist");
    Assertions.assertEquals(0, counter.getBlibliSkuCounter(), "Blibli SKU counter should not be incremented");
    Assertions.assertTrue(tempErrorMessage.toString().isEmpty(),
        "Error message should be empty when key does not exist");
  }

  @Test
  void testValidateMandatoryColumn_BlibliProductSku() throws Exception {
    // Test case for BLIBLI_PRODUCT_SKU parameter
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_PRODUCT_SKU, ""); // Empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_PRODUCT_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertFalse(result, "Should return false when BLIBLI_PRODUCT_SKU is empty");
    Assertions.assertEquals(1, counter.getBlibliProductSkuCounter(), "Blibli Product SKU counter should be incremented");
    Assertions.assertTrue(tempErrorMessage.toString().contains(BulkUpdateServiceUtil.BLIBLI_PRODUCT_SKU_BLANK),
        "Error message should contain BLIBLI_PRODUCT_SKU_BLANK");
  }

  @Test
  void testValidateMandatoryColumn_GenericParameter() throws Exception {
    // Test case for generic parameter (not BLIBLI_SKU or BLIBLI_PRODUCT_SKU)
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    String genericParameter = "GENERIC_FIELD";
    cleanData.put(genericParameter, ""); // Empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, genericParameter);
    
    Assertions.assertFalse(result, "Should return false when generic mandatory column is empty");
    Assertions.assertTrue(tempErrorMessage.toString().contains("Mandatory field " + genericParameter + " is blank"),
        "Error message should contain generic error message");
    Assertions.assertTrue(tempErrorMessage.toString().endsWith(BulkUpdateServiceUtil.AND_SYMBOL),
        "Error message should end with AND_SYMBOL");
  }

  @Test
  void testValidateMandatoryColumn_SuccessFlagAlreadyFalse() throws Exception {
    // Test case where successFlag is already false - should remain false
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, ""); // Empty value
    boolean successFlag = false; // Already false
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertFalse(result, "Should return false when successFlag is already false");
    Assertions.assertEquals(1, counter.getBlibliSkuCounter(), "Blibli SKU counter should be incremented");
    Assertions.assertTrue(tempErrorMessage.toString().contains(BulkUpdateServiceUtil.BLIBLI_SKU_BLANK),
        "Error message should contain BLIBLI_SKU_BLANK");
  }

  @Test
  void testValidateMandatoryColumn_ErrorCountExceedsLimit() throws Exception {
    // Test case where error count exceeds ERROR_COUNT (100) - should not add to error message
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    // Set counter to ERROR_COUNT (100) so next increment will exceed it
    for (int i = 0; i < BulkUpdateServiceUtil.ERROR_COUNT; i++) {
      counter.incrementBlibliSkuCounter();
    }
    Assertions.assertEquals(BulkUpdateServiceUtil.ERROR_COUNT, counter.getBlibliSkuCounter(),
        "Counter should be at ERROR_COUNT");
    
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, ""); // Empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertFalse(result, "Should return false when mandatory column is empty");
    Assertions.assertEquals(BulkUpdateServiceUtil.ERROR_COUNT + 1, counter.getBlibliSkuCounter(),
        "Blibli SKU counter should be incremented beyond ERROR_COUNT");
    // When counter exceeds ERROR_COUNT, shouldAddToMessage returns false, so error message should not be added
    Assertions.assertTrue(tempErrorMessage.toString().isEmpty(),
        "Error message should be empty when error count exceeds ERROR_COUNT");
  }

  @Test
  void testValidateMandatoryColumn_ErrorCountAtLimit() throws Exception {
    // Test case where error count is exactly at ERROR_COUNT (100) - should still add to error message
    BulkUpdateErrorCounter counter = new BulkUpdateErrorCounter();
    // Set counter to ERROR_COUNT - 1 so next increment will be exactly at ERROR_COUNT
    for (int i = 0; i < BulkUpdateServiceUtil.ERROR_COUNT - 1; i++) {
      counter.incrementBlibliSkuCounter();
    }
    Assertions.assertEquals(BulkUpdateServiceUtil.ERROR_COUNT - 1, counter.getBlibliSkuCounter(),
        "Counter should be at ERROR_COUNT - 1");
    
    Map<String, String> cleanData = new LinkedHashMap<>();
    cleanData.put(BulkParameters.BLIBLI_SKU, ""); // Empty value
    boolean successFlag = true;
    StringBuilder tempErrorMessage = new StringBuilder();
    String parameter = BulkParameters.BLIBLI_SKU;
    
    // Use reflection to call the private static method
    java.lang.reflect.Method method = BulkUpdateServiceUtil.class.getDeclaredMethod(
        "validateMandatoryColumn",
        BulkUpdateErrorCounter.class,
        Map.class,
        boolean.class,
        StringBuilder.class,
        String.class);
    method.setAccessible(true);
    boolean result = (boolean) method.invoke(null, counter, cleanData, successFlag, tempErrorMessage, parameter);
    
    Assertions.assertFalse(result, "Should return false when mandatory column is empty");
    Assertions.assertEquals(BulkUpdateServiceUtil.ERROR_COUNT, counter.getBlibliSkuCounter(),
        "Blibli SKU counter should be exactly at ERROR_COUNT");
    // When counter is exactly at ERROR_COUNT, shouldAddToMessage returns true (counter <= ERROR_COUNT)
    Assertions.assertTrue(tempErrorMessage.toString().contains(BulkUpdateServiceUtil.BLIBLI_SKU_BLANK),
        "Error message should contain BLIBLI_SKU_BLANK when counter is at ERROR_COUNT");
  }

  @Test
  void testGetProductCodeFromItemCode_NormalCase() {
    // Test case with normal item code format: "PRD-001-ITEM-001"
    // Should return "PRD-001" (substring from start to second hyphen)
    String itemCode = "PRD-001-ITEM-001";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    Assertions.assertEquals("PRD-001", result, "Should return substring from start to second hyphen");
  }

  @Test
  void testGetProductCodeFromItemCode_ExactlyTwoHyphens() {
    // Test case with exactly two hyphens: "PRD-001-ITEM"
    // Should return "PRD-001" (substring from start to second hyphen)
    String itemCode = "PRD-001-ITEM";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    Assertions.assertEquals("PRD-001", result, "Should return substring from start to second hyphen when there are exactly two hyphens");
  }

  @Test
  void testGetProductCodeFromItemCode_MoreThanTwoHyphens() {
    // Test case with more than two hyphens: "PRD-001-ITEM-001-SUFFIX"
    // Should return "PRD-001" (substring from start to second hyphen)
    String itemCode = "PRD-001-ITEM-001-SUFFIX";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    Assertions.assertEquals("PRD-001", result, "Should return substring from start to second hyphen, ignoring additional hyphens");
  }

  @Test
  void testGetProductCodeFromItemCode_OneHyphen() {
    // Test case with only one hyphen: "PRD-001"
    // This would cause StringIndexOutOfBoundsException when trying to find second hyphen
    String itemCode = "PRD-001";
    // When there's only one hyphen, secondHyphenIndex would be -1, causing substring to fail
    // Actually wait, if itemCode is "PRD-001", firstHyphenIndex = 3, secondHyphenIndex = -1 (not found)
    // substring(0, -1) would throw StringIndexOutOfBoundsException
    Assertions.assertThrows(StringIndexOutOfBoundsException.class, () -> {
      BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    }, "Should throw StringIndexOutOfBoundsException when there's only one hyphen");
  }

  @Test
  void testGetProductCodeFromItemCode_NoHyphens() {
    // Test case with no hyphens: "PRD001"
    // This would cause StringIndexOutOfBoundsException when trying to find first hyphen
    String itemCode = "PRD001";
    // When there are no hyphens, firstHyphenIndex = -1, secondHyphenIndex would be -1
    // substring(0, -1) would throw StringIndexOutOfBoundsException
    Assertions.assertThrows(StringIndexOutOfBoundsException.class, () -> {
      BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    }, "Should throw StringIndexOutOfBoundsException when there are no hyphens");
  }

  @Test
  void testGetProductCodeFromItemCode_EmptyString() {
    // Test case with empty string
    String itemCode = "";
    // Empty string: firstHyphenIndex = -1, secondHyphenIndex = -1
    // substring(0, -1) would throw StringIndexOutOfBoundsException
    Assertions.assertThrows(StringIndexOutOfBoundsException.class, () -> {
      BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    }, "Should throw StringIndexOutOfBoundsException when string is empty");
  }

  @Test
  void testGetProductCodeFromItemCode_StartsWithHyphen() {
    // Test case starting with hyphen: "-PRD-001"
    String itemCode = "-PRD-001";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    // firstHyphenIndex = 0, secondHyphenIndex = 4 (position of second '-')
    // substring(0, 4) = "-PRD" (substring end index is exclusive, so it doesn't include the second hyphen)
    Assertions.assertEquals("-PRD", result, "Should return substring from start to second hyphen (exclusive)");
  }

  @Test
  void testGetProductCodeFromItemCode_ConsecutiveHyphens() {
    // Test case with consecutive hyphens: "PRD--001-ITEM"
    String itemCode = "PRD--001-ITEM";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    // firstHyphenIndex = 3, secondHyphenIndex = 4 (next hyphen after firstHyphenIndex + 1)
    // substring(0, 4) = "PRD-"
    Assertions.assertEquals("PRD-", result, "Should return substring from start to second hyphen (handling consecutive hyphens)");
  }

  @Test
  void testGetProductCodeFromItemCode_RealWorldExample() {
    // Test case with a realistic item code format used in the codebase
    // Based on EANProductLevel4BulkUpdateServiceBean usage
    String itemCode = "BP123-PRD-001-ITEM-001";
    String result = BulkUpdateServiceUtil.getProductCodeFromItemCode(itemCode);
    // firstHyphenIndex = 5 (position of first '-')
    // secondHyphenIndex = 9 (position of second '-')
    // substring(0, 9) = "BP123-PRD"
    Assertions.assertEquals("BP123-PRD", result, "Should extract product code from realistic item code format");
  }

}
