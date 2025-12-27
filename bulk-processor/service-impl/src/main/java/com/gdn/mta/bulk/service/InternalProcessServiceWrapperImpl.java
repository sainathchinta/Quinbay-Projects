package com.gdn.mta.bulk.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.models.CampaignProductUpdateDto;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkSkuLevelRebateRequestData;
import com.gdn.mta.bulk.request.InternalBrandUpdateNotes;
import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import com.gdn.partners.bulk.util.BulkIPRProductsParameter;
import com.gdn.mta.bulk.repository.campaign.CampaignRepository;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.util.Pair;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.dto.BulkInternalProcessDataGenerationDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessPendingDataDTO;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.BulkUpdateSuccessDTO;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.BulkUpdateErrorCounter;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.models.InternalBulkUploadRequest;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.SalesCategoryUpdateRequest;
import com.gdn.mta.bulk.models.VendorBulkAssignmentRequest;
import com.gdn.mta.bulk.models.download.BrandAuthorisationRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.ClusterReviewFeedbackRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssignAutoApprovedProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssigneeMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRebateRequestData;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.ClusterItemErrorListResponse;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.MasterSkuItemsRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.repository.pcb.ProductAttributeRepository;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.bulk.util.ResponseHelper;
import com.gdn.partners.bulk.util.BulkMasterSkuUploadParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.partners.bulk.util.MasterSkuActions;
import com.gdn.partners.bulk.util.SalesCategoryUpdateConstants;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.gdn.partners.bulk.util.VendorProductDataBulkParameters;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.enums.ApiErrorCode;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class InternalProcessServiceWrapperImpl implements InternalProcessServiceWrapper {

  List<String> DELETE_STATUS = Arrays
      .asList(ProcessStatus.CANCELLED.name(), ProcessStatus.COMPLETED.name(), ProcessStatus.PARTIAL_COMPLETED.name(),
          ProcessStatus.FAILED.name());
  Collection<String> INTERNAL_PROCESS_TYPE_SET = Set.of(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name(),
      BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(),
      BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name(), BulkInternalProcessType.BRAND_AUTH_ADD.name(),
      BulkInternalProcessType.BRAND_AUTH_DELETE.name(),BulkInternalProcessType.BULK_APPROVAL.name(),
      BulkInternalProcessType.BULK_REJECTION.name(), BulkInternalProcessType.BULK_PRICE_UPDATE.name(),
      BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name(), BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name());
  Collection<String> INTERNAL_PROCESS_TYPES_INLINE_SAVE_SET = Set.of(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
  private static final String X_PRODUCT_ERROR_MESSAGE = "Failed to fetch data from x-product for the product";
  private static final String SUSPENDED_ERROR_MESSAGE = "Product is suspended";
  private static final String TAKEN_DOWN_ERROR_MESSAGE = "Product is taken down";
  private static final String DELETED_ERROR_MESSAGE = "Product is deleted";
  private static final String ARCHIVED_ERROR_MESSAGE = "All items of this product are archived";
  private static final String EXCEPTION_ERROR_MESSAGE = "Error while creating product : ";
  private static final String PROTECTED_BRAND_MESSAGE = "This seller doesn't have authorisation "
    + "for the selected brand. Please update brand and try again";
  private static final String ASSIGNEE_ERROR_MESSAGE = "Pengguna tidak memiliki akses untuk "
    + "tindakan ini.";
  private static final String DATA_NOT_FOUND_ERROR_MESSAGE = "Master SKU tidak valid.";

  private static final String LIST_REGEX = "\\s+";
  private static final String FILTER_TYPE = "ALL";
  private static final String DOCUMENT_FILTER_TYPE = "ALL";

  private static final String YES = "YES";

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private BulkProductSuspensionService bulkProductSuspensionService;

  @Autowired
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private FileStorageServiceBean fileStorageServiceBean;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private PartnersEngineOutboundService partnersEngineOutboundService;

  @Autowired
  private VendorProductBulkAssignService vendorProductBulkAssignService;

  @Autowired
  private BrandAuthorisationService brandAuthorisationService;

  @Autowired
  private FbbConsignmentService fbbConsignmentService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  @Autowired
  private BulkVendorActionService bulkVendorActionService;

  @Autowired
  private BrandUpdateService brandUpdateService;

  @Autowired
  private MasterSkuReviewOutboundService masterSkuReviewOutboundService;

  @Autowired
  private MasterSkuItemsRepository masterSkuItemsRepository;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private BulkMasterSkuReviewService bulkMasterSkuReviewService;

  @Autowired
  private ProductAnalyticsOutboundService productAnalyticsOutboundService;

  @Autowired
  private BulkAutoApprovedProductsService bulkAutoApprovedProductsService;

  @Autowired
  private BulkPriceUpdateService bulkPriceUpdateService;

  @Autowired
  private ProductLevel3BulkUpdateServiceBean productLevel3BulkUpdateServiceBean;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private BulkRebateUpdateService bulkRebateUpdateService;

  @Autowired
  private BulkSkuLevelRebateService bulkSkuLevelRebateService;

  @Autowired
  private BulkPriceUpdateNewService bulkPriceUpdateNewService;

  @Autowired
  private CampaignRepository campaignRepository;

  @Autowired
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private BulkIPRProductService bulkIPRProductService;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Value("${sales.catalog.code}")
  private String salesCatalogCode;

  @Value("${partners.engine.size}")
  private int partnerEngineSize;

  @Value("${msku.role.code}")
  private String mskuRoleCode;

  @Value("${child.categories.max.size}")
  private int childCategoryMaxSize;

  @Value("${brand.auth.end.date.years.add}")
  private int brandAuthEndYear;

  @Value("${vendor.code}")
  private String vendorCode;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${bulk.price.update.max.allowed.rows}")
  private int bulkPriceUpdateMaxRows;

  @Value("${bulk.rebate.max.allowed.rows}")
  private int bulkRebateMaxRows;

  @Value("${bulk.product.type.tagging.max.rows}")
  private int bulkProductTypeTaggingMaxRows;

  @Value("${bulk.sku.level.rebate.max.rows}")
  private int bulkSkuLevelRebateMaxRows;

  @Value("${bulk.new.price.update.max.rows}")
  private int bulkPriceUpdateNewMaxRows;

  @Value("${internal.bulk.update.item.pickup.point.list.fetch.size}")
  private int internalItemPickupPointListFetchSize;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${bulk.price.update.allowed.sellers}")
  private String bulkPriceUpdateAllowedSellers;

  @Value("${validate.bulk.max.number.of.rows}")
  private boolean validateBulkMaxNumberOfRows;

  @Value("${bulk.max.number.of.rows}")
  private int bulkMaxNumberOfRows;

  @Value("${ipr.role.code.reviewer}")
  private String iprRoleCodeReviewer;

  @Value("${bulk.brand.update.products.fetch.size}")
  private int bulkBrandUpdateProductsFetchSize;

@Autowired
private BulkProductTypeTaggingUpdateService bulkProductTypeTaggingUpdateService;

  @Async
  @Override
  public void processNewInternalProcessRequest(String storeId, String userName, String processType) {
    log.info("Started processing new internal process request");
    Integer fileBatchSize = getBatchSizeByProcessType(storeId, processType);
    Set<String> processingRequests = Collections.synchronizedSet(new HashSet<>());
    Page<BulkInternalProcess> bulkInternalProcesses =
      BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType) ?
        internalProcessService.getAllBulkInternalProcessByStatusOrderByDateAsc(storeId,
          ProcessStatus.PENDING.name(), PageRequest.of(0, fileBatchSize), processType) :
        internalProcessService.getAllBulkInternalProcessByStatus(storeId,
          ProcessStatus.PENDING.name(), PageRequest.of(0, fileBatchSize), processType);
    List<BulkInternalProcess> bulkInternalProcessList =  new ArrayList<>();

    if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      bulkInternalProcessList = bulkInternalProcesses.getContent();
    } else {
      bulkInternalProcessList = setBulkInterProcessToPickedState(bulkInternalProcesses.getContent());
    }

    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcessList) {
      try {
        List<BulkInternalProcessData> bulkInternalProcessData = new ArrayList<>();
        if (BulkInternalProcessType.FBB_L5_CREATE.name()
          .equals(bulkInternalProcess.getProcessType())) {
          bulkInternalProcessData = internalProcessService
            .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(storeId,
              bulkInternalProcess.getInternalProcessRequestCode(), BulkProcessData.STATUS_PENDING);
        }
        else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())) {
          BulkInternalProcess bulkInternalProcessInProgress =
            internalProcessService.findFirstByStoreIdAndProcessTypeAndStatusIn(storeId,
              BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name(),
              Stream.of(ProcessStatus.IN_PROGRESS.name(), ProcessStatus.PUBLISHED.name(),
                  ProcessStatus.PICKED.name())
                .collect(Collectors.toList()));
          if (Objects.nonNull(bulkInternalProcessInProgress)) {
            log.info("Skipping vendor auto assignments processing for {} . Unfinished assignments "
              + "exist.", bulkInternalProcess.getInternalProcessRequestCode());
            return;
          }
          else {
            bulkInternalProcess.setStatus(ProcessStatus.PICKED.name());
            bulkInternalProcess.setUpdatedDate(new Date());
            bulkInternalProcess = internalProcessService.saveInternalProcess(bulkInternalProcess);
            processVendorAutoAssignments(storeId,bulkInternalProcess, userName, processingRequests);
          }
        } else if (BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name()
            .equals(bulkInternalProcess.getProcessType()) || BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()
            .equals(bulkInternalProcess.getProcessType())) {
          processInternalBrandUpdates(storeId,bulkInternalProcess,userName);
        }
        else {
          bulkInternalProcessData = getInternalProcessDataFromExcel(storeId, bulkInternalProcess, userName);
          internalProcessService.saveInternalProcessData(bulkInternalProcessData);
        }
        if (!INTERNAL_PROCESS_TYPE_SET.contains(bulkInternalProcess.getProcessType())) {
          RequestHelper.updateInternalProcessStatusAndSize(bulkInternalProcess, bulkInternalProcessData, userName);
        }
        if (!INTERNAL_PROCESS_TYPES_INLINE_SAVE_SET.contains(bulkInternalProcess.getProcessType())) {
          internalProcessService.saveInternalProcess(bulkInternalProcess);
        }
        log.info("Data saved for internal process {} ", bulkInternalProcess);
      }  catch (ApplicationException e) {
        log.error("Validation error while processing internal process copy internal-process-request-code : {} ",
            bulkInternalProcess.getInternalProcessRequestCode(), e);
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
        internalProcessService.saveInternalProcess(bulkInternalProcess);
        mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, bulkInternalProcess.getStatus());
      } catch (Exception e) {
        log.error("error while processing internal process internal-process-request-code : {} ",
            bulkInternalProcess.getInternalProcessRequestCode(), e);
        bulkInternalProcess.setNotes(Constant.SYSTEM_ERROR);
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
        internalProcessService.saveInternalProcess(bulkInternalProcess);
      }
    }
    log.info("Done processing Internal process request");
  }

  private void processInternalBrandUpdates(String storeId,
      BulkInternalProcess bulkInternalProcess, String userName) throws IOException {
    InternalBrandUpdateNotes brandUpdateNotes =
        objectMapper.readValue(bulkInternalProcess.getNotes(), InternalBrandUpdateNotes.class);

    String destinationBrandCode = brandUpdateNotes.getDestinationBrandCode();
    Map<String, Boolean> sellerCodeAutorisationMap = new HashMap<>();
    boolean brandNameUpdate = BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name()
        .equals(bulkInternalProcess.getProcessType());
    int page = 0;
    long totalRecords;
    do {
      GdnRestListResponse<ProductAndBrandResponse> response =
          pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(page,
              brandUpdateNotes.getSourceBrandName(), storeId);

      List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();

      for (ProductAndBrandResponse product : response.getContent()) {
        String productCode = product.getProductCode();
        Set<String> businessPartnerCodes = product.getBusinessPartnerCodes();

        boolean brandAuthorisedForUpdate = true;

        BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
        if (brandNameUpdate) {
          // For brand name updates, only the first entry is marked as a brand name update.
          // Brand-level details are therefore updated only once.
          bulkInternalProcessData.setData(
              BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name());
          brandNameUpdate = false;
        }
        else {
          checkBrandAuthorisedForSellerOrNot(storeId, businessPartnerCodes, sellerCodeAutorisationMap,
              destinationBrandCode);
        }
        RequestHelper.setBasicInternalProcessDataDetailsForInternalBrandUpdate(storeId, bulkInternalProcess,
            userName, bulkInternalProcessData, productCode, brandAuthorisedForUpdate,
            bulkInternalProcessDataList, destinationBrandCode);
      }
      totalRecords = response.getPageMetaData().getTotalRecords();
      page++;
      internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    } while ((long) page * bulkBrandUpdateProductsFetchSize < totalRecords);
  }

  private boolean checkBrandAuthorisedForSellerOrNot(String storeId,
      Set<String> businessPartnerCodes, Map<String, Boolean> sellerCodeAuthorisationMap,
      String destinationBrandCode) {
    return Optional.ofNullable(businessPartnerCodes).orElseGet(HashSet::new).stream()
        .allMatch(bpCode -> {
          if (Objects.nonNull(sellerCodeAuthorisationMap.get(bpCode))) {
            return sellerCodeAuthorisationMap.get(bpCode);
          }
          try {
            boolean authorised =
                checkProtectedBrandCreationAuthorisation(storeId, destinationBrandCode, bpCode);
            sellerCodeAuthorisationMap.put(bpCode, authorised);
            return authorised;
          } catch (Exception e) {
            log.error("no authorisation to update the brand : {}", destinationBrandCode, e);
            sellerCodeAuthorisationMap.put(bpCode, false);
            return false;
          }
        });
  }

  private List<BulkInternalProcess> setBulkInterProcessToPickedState(List<BulkInternalProcess> bulkInternalProcessList) {
    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcessList) {
      bulkInternalProcess.setStatus(ProcessStatus.PICKED.name());
    }
    return internalProcessService.saveInternalProcesses(bulkInternalProcessList);
  }

  private void processVendorAutoAssignments(String storeId,
    BulkInternalProcess bulkInternalProcess, String userName, Set<String> processingRequests) throws IOException {
    List<String> assignList = new ArrayList<>();
    int errorCount = 0;
    List<BulkInternalProcessData> processedDataList = new ArrayList<>();
    try {
        processingRequests.add(bulkInternalProcess.getInternalProcessRequestCode());
      if (Objects.nonNull(bulkInternalProcess.getNotes())) {
        assignList = Arrays.asList(
          bulkInternalProcess.getNotes().substring(1, bulkInternalProcess.getNotes().length() - 1)
            .replaceAll(LIST_REGEX, "").split(","));
      }
      log.info("Assignee List for Vendor Auto Assignment is {} ", assignList);
      int counterForAssignment = assignList.size();
      int fetchBatchSizeForAutoAssignment =
        (int) Math.ceil((double) bulkInternalProcess.getTotalCount() / counterForAssignment);
      while (counterForAssignment > 0) {
        List<VendorBulkAssignmentRequest> vendorBulkAssignmentRequests = new ArrayList<>();
        List<String> productCodes = new ArrayList<>();
        List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
        try {
          VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
            objectMapper.readValue(bulkInternalProcess.getFileName(),
              VendorAutoAssignmentFilterRequest.class);
          GdnRestListResponse<ProductCodeResponse> response =
            productDistributionTaskRepository.fetchProductsForAutoAssignment(bulkInternalProcess.getInternalProcessRequestCode(), userName,
              assignList.size() - counterForAssignment, fetchBatchSizeForAutoAssignment,
              RequestHelper.toBoostedProductFilterRequest(vendorAutoAssignmentFilterRequest));
          productCodes = response.getContent().stream().map(ProductCodeResponse::getProductCodes)
            .collect(Collectors.toList());

          Map<String, String> productCodeAssigneeMap = assignRoundRobin(productCodes, assignList);

          productCodeAssigneeMap.forEach(
            (productCode, assignee) -> vendorBulkAssignmentRequests.add(
              VendorBulkAssignmentRequest.builder().productCode(productCode)
                .assignedBy(bulkInternalProcess.getCreatedBy()).assignedTo(assignee)
                .assignmentType(Constant.VENDOR_AUTO_ASSIGNMENT).build()));


          for (VendorBulkAssignmentRequest vendorBulkAssignmentRequest : vendorBulkAssignmentRequests) {
            BulkInternalProcessData bulkInternalProcessDataForAutoAssignment = new BulkInternalProcessData();
            bulkInternalProcessDataForAutoAssignment.setCreatedDate(new Date());
            bulkInternalProcessDataForAutoAssignment.setData(new ObjectMapper().writeValueAsString(vendorBulkAssignmentRequest));
            bulkInternalProcessDataForAutoAssignment.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
            bulkInternalProcessDataForAutoAssignment.setStatus(ProcessStatus.PENDING.name());
            RequestHelper.setBulkInternalProcessDataBasicDetails(storeId, bulkInternalProcess.getCreatedBy(), bulkInternalProcess,
              bulkInternalProcessDataForAutoAssignment);
            bulkInternalProcessDataList.add(bulkInternalProcessDataForAutoAssignment);
          }

          bulkInternalProcessDataList.forEach(
            autoAssignmentInternalData -> internalProcessService.saveBulkInternalProcessData(
              autoAssignmentInternalData));
          log.info("Vendor Auto Assignment : Total products requested : {} , total products "
              + "fetched : {}", bulkInternalProcess.getTotalCount(),
            bulkInternalProcessDataList.size());
          processedDataList.addAll(bulkInternalProcessDataList);
          errorCount += fetchBatchSizeForAutoAssignment - productCodes.size();
        } catch (Exception e) {
          log.error("error while processing internal process internal-process-request-code : {} ",
            bulkInternalProcess.getInternalProcessRequestCode(), e);
          bulkInternalProcess.setErrorCount(
            fetchBatchSizeForAutoAssignment - vendorBulkAssignmentRequests.size());
          errorCount += fetchBatchSizeForAutoAssignment;
        } finally {
            counterForAssignment--;
        }
        if (bulkInternalProcess.getTotalCount().equals(errorCount)) {
          bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
          log.info("Vendor Auto Assignment Failed with error count : {} for process code : {} ",
            errorCount, bulkInternalProcess.getInternalProcessRequestCode());
        } else {
          RequestHelper.updateInternalProcessForVendorAutoAssignment(bulkInternalProcess,
            processedDataList, userName, errorCount);
          log.info("Vendor Auto Assignment was success with error count : {} for process code : {} ",
            errorCount, bulkInternalProcess.getInternalProcessRequestCode());
        }
      }}
    finally {
      processingRequests.remove(bulkInternalProcess.getInternalProcessRequestCode());
      log.info("Processing Request for vendor Assignment for request : {} is : {} ",
        bulkInternalProcess.getInternalProcessRequestCode(), processingRequests);
      internalProcessService.saveInternalProcess(bulkInternalProcess);
      }
  }

  private Map<String, String> assignRoundRobin(List<String> productCodes, List<String> assignList) {
    List<String> shuffledProductCodes = new ArrayList<>(productCodes);
    Collections.shuffle(shuffledProductCodes);

    Map<String, String> productCodeAssigneeMap =
      IntStream.range(0, shuffledProductCodes.size()).boxed().collect(
        Collectors.toMap(i -> shuffledProductCodes.get(i),
          i -> assignList.get(i % assignList.size()), (oldAssignee, newAssignee) -> newAssignee,
          LinkedHashMap::new));
    return productCodeAssigneeMap;
  }

  @Override
  public void processInternalProcessDataRequest(String storeId, String requestId, String username, String processType) {
    try {
      Integer totalBatchSize = getTotalBatchSizeByProcessType(storeId, processType);
      Integer fileBatchSize = getBatchSizeByProcessType(storeId, processType);
      Integer fetchBatchSize = getFetchBatchSizeByProcessType(storeId, processType);
      Integer numberOfPages = (int) Math.ceil((double) totalBatchSize / fetchBatchSize);
      do {
        log.info("numberOfPages for processInternalProcessDataRequest. requestId : {} , numberOfPages : {} ", requestId,
            numberOfPages);
        Page<BulkInternalProcess> bulkInternalProcesses =
            internalProcessService.getAllBulkInternalProcessByStatus(storeId, ProcessStatus.IN_PROGRESS.name(),
                PageRequest.of(0, fileBatchSize), processType);
        if (CollectionUtils.isEmpty(bulkInternalProcesses.getContent())) {
          log.info("No new file found to process for requestId : {} and processType {} ", requestId, processType);
          return;
        }
        log.info(
            "Scheduler 2: started processing internal process data fileBatch : {} batchSize : {} bulkInternalProcesses : {}",
            fileBatchSize, fetchBatchSize, bulkInternalProcesses.getContent());
        if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
            processStoreCopyRequest(storeId, bulkInternalProcesses, fetchBatchSize, processType);
        } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
          processSalesCategoryUpdateRequest(storeId, bulkInternalProcesses, fetchBatchSize);
        } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
          processInternalBulkUploadRequest(storeId, bulkInternalProcesses, fetchBatchSize);
        } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
          bulkProductSuspensionService.processToPublishForSuspension(storeId, requestId, bulkInternalProcesses);
        } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)
          || BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
          processVendorBulkAssignmentRequests(storeId, bulkInternalProcesses, fetchBatchSize);
        } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
          processDeleteBulkAuthorisationRequests(storeId, bulkInternalProcesses, fetchBatchSize);
        } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
          bulkConfigurationUpdateService.processToPublishForConfigUpdate(storeId, requestId, bulkInternalProcesses);
        } else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
          fbbConsignmentService.publishL5CreateRows(storeId, requestId, bulkInternalProcesses);
        } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)
            || BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
          restrictedKeywordService.publishRestrictedKeywordBulkUpload(storeId, bulkInternalProcesses.getContent(),
              fetchBatchSize);
        } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)
            || BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
          processBulkBrandAuthorisationRequests(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.BRAND_UPDATE.name().equals(processType)) {
          brandUpdateService.publishBrandUpdateEvent(storeId, bulkInternalProcesses.getContent(), fetchBatchSize);
        } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)
          || BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
          processBulkApprovalRejectionRequests(storeId, bulkInternalProcesses, fetchBatchSize,
            username);
        } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
          processBulkAssigneeRequests(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
          processBulkMasterSkuReviewRequests(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(processType)) {
          processAutoApprovedProductBulkAssignRequests(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
          processBulkPriceUpdateRequests(storeId, bulkInternalProcesses, fetchBatchSize, username,
              kafkaTopicProperties.getBulkInternalPriceUpdateEvent());
        } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
          processBulkRebateUpload(storeId, bulkInternalProcesses, fetchBatchSize, username);
        }
        else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)){
          processBulkProductTypeTaggingRequests(storeId, bulkInternalProcesses, fetchBatchSize,
            username);
        } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
          processBulkIPRProductAddReview(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
          processBulkSkuLevelRebateUpload(storeId, bulkInternalProcesses, fetchBatchSize, username);
        } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
          processBulkPriceUpdateRequests(storeId, bulkInternalProcesses, fetchBatchSize, null,
              kafkaTopicProperties.getBulkPriceUpdateNewEvent());
        }
        else if (BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name().equals(processType) || BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name().equals(processType)){
          processInternalBrandUpdateData(storeId,bulkInternalProcesses,fetchBatchSize,username);
        }
        List<BulkInternalProcess> bulkInternalProcessList =
            updateInternalProcessStatus(bulkInternalProcesses.getContent(), ProcessStatus.PUBLISHED.name(), username,
                storeId);
        if (CollectionUtils.isNotEmpty(bulkInternalProcessList)) {
          internalProcessService.saveInternalProcesses(bulkInternalProcessList);
        }
        log.info("Scheduler 2: Done processing internal process data");
        numberOfPages = numberOfPages - 1;
      } while (numberOfPages > 0);
    } catch (Exception e) {
      log.error("error while processing internal process data ", e);
    }
  }


  private void processBulkProductTypeTaggingRequests(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
    int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList = internalProcessService
      .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
        bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
        ProcessStatus.PENDING.name(), fetchBatchSize);
    Map<String, String> requestCodeXCreatedBy =
      bulkInternalProcesses.stream().collect(Collectors.toMap(BulkInternalProcess::getInternalProcessRequestCode, BulkInternalProcess::getCreatedBy));
    int maxNumberOfL5sInUpdateRequest = getSystemParameterConfigValue(storeId,
      SystemParameterConfigNames.PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE);
    Map<String, List<BulkInternalProcessData>> bulkInternalProcessGroupedData = bulkInternalProcessDataList.stream().collect(
        Collectors.groupingBy(BulkInternalProcessData::getInternalProcessRequestCode));
    for (Map.Entry<String, List<BulkInternalProcessData>> groupedData : bulkInternalProcessGroupedData.entrySet()) {
      List<List<BulkInternalProcessData>> partition =
        Lists.partition(groupedData.getValue(), maxNumberOfL5sInUpdateRequest);
      partition.forEach(bulkInternalProcessData ->  publishBulkProductTypeTaggingUpdateEvent(storeId,
        bulkInternalProcessData, requestCodeXCreatedBy.get(groupedData.getKey()), groupedData.getKey()));
    }
  }

  private void publishBulkProductTypeTaggingUpdateEvent(String storeId,
    List<BulkInternalProcessData> bulkInternalProcessDataList,
    String username, String internalProcessRequestCode) {
    bulkInternalProcessDataList.forEach(bulkInternalProcessData -> bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name()));
    internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    log.info("Published : {} for request : {} ", kafkaTopicProperties.getBulkProductTypeTaggingUpdateEvent(),
      internalProcessRequestCode);
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
      RequestHelper.toInternalBulkUploadDataDomainEventModel(
        bulkInternalProcessDataList.stream().map(BulkInternalProcessData::getId).collect(Collectors.toList()),
        bulkInternalProcessDataList.get(0).getProcessType(), storeId, username);
    kafkaProducer.send(kafkaTopicProperties.getBulkProductTypeTaggingUpdateEvent(),
      internalProcessRequestCode, internalBulkUploadDataDomainEventModel);
  }

  private void processInternalBrandUpdateData(String storeId,
      Page<BulkInternalProcess> bulkInternalProcesses, Integer fetchBatchSize, String username)
      throws Exception {
    List<BulkInternalProcessData> bulkInternalProcessDatas =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
                .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);
    if (CollectionUtils.isNotEmpty(bulkInternalProcessDatas)) {
      for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDatas) {
        publishInternalBrandUpdate(storeId, username, bulkInternalProcessData);
      }
    }
  }

  private void publishInternalBrandUpdate(String storeId, String username,
      BulkInternalProcessData bulkInternalProcessData) throws Exception {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getInternalBrandUpdateEvent(),
        RequestHelper.toInternalBrandUpdateEventModel(bulkInternalProcessData, storeId,
            bulkInternalProcessData.getUpdatedBy()));
  }

  private void processStoreCopyRequest(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize, String processType) {
    Map<String, List<BulkInternalProcessPendingDataDTO>> bulkInternalProcessDataParentCodeMap = internalProcessService
        .getAllBulkInternalProcessDataByStatusAndBatchSize(ProcessStatus.PENDING.name(), fetchBatchSize, processType,
            storeId, bulkInternalProcesses.getContent());
    bulkInternalProcessDataParentCodeMap.values().forEach(
        bulkInternalProcessPendingDataDTOS -> publishStoreCopyProductAndUpdateStatus(bulkInternalProcessPendingDataDTOS,
            storeId));
  }

  private void processSalesCategoryUpdateRequest(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize) {
    List<BulkInternalProcessData> bulkInternalProcessDatas = internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name(), fetchBatchSize);
    bulkInternalProcessDatas.forEach(
        bulkInternalProcessData -> publishUpdateSalesCategoryAndUpdateStatus(storeId, bulkInternalProcessData));
  }

  private void processInternalBulkUploadRequest(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize) {
    Map<String, String> internalProcessIdAndUserName = bulkInternalProcesses.getContent()
        .stream().collect(Collectors.toMap(BulkInternalProcess::getId, BulkInternalProcess::getCreatedBy));
    List<BulkInternalProcessData> bulkInternalProcessDatas = internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name(), fetchBatchSize);
    bulkInternalProcessDatas.forEach(bulkInternalProcessData -> publishInternalBulkUploadAndUpdateStatus(
        storeId, internalProcessIdAndUserName.getOrDefault(bulkInternalProcessData.getInternalProcessRequestId(),
            Constant.DEFAULT), bulkInternalProcessData));
  }

  private void processVendorBulkAssignmentRequests(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize) {
    List<BulkInternalProcessData> bulkInternalProcessDatas = internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name() ,fetchBatchSize);
    bulkInternalProcessDatas.stream()
        .forEach(
            bulkInternalProcessData -> publishVendorBulkAssignmentAndUpdateStatus(storeId, bulkInternalProcessData));
  }

  private void processDeleteBulkAuthorisationRequests(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize) {
    List<BulkInternalProcessData> bulkInternalProcessDatas = internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name() ,fetchBatchSize);
    Optional.ofNullable(bulkInternalProcessDatas).orElse(new ArrayList<>()).stream().forEach(
            bulkInternalProcessData -> publishDeleteBrandAuthorisationAndUpdateStatus(storeId, bulkInternalProcessData));
  }

  private void processBulkBrandAuthorisationRequests(String storeId,
      Page<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDatas =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
                .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);
    if (CollectionUtils.isNotEmpty(bulkInternalProcessDatas)) {
      bulkInternalProcessDatas.forEach(bulkInternalProcessData -> {
        validateAndPublishBulkBrandAuthorisationRequests(storeId, username,
            bulkInternalProcessData);
      });
    }
  }

  private void validateAndPublishBulkBrandAuthorisationRequests(String storeId, String username,
      BulkInternalProcessData bulkInternalProcessData) {
    try {
      BrandAuthAddRequestData brandAuthAddRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), BrandAuthAddRequestData.class);
      String brandCodeAndSellerCode = brandAuthAddRequestData.getBrandCode() + Constant.UNDERSCORE
          + brandAuthAddRequestData.getSellerCode();
      publishBulkBrandAuthorisationAndUpdateStatus(storeId, bulkInternalProcessData, username,
          brandCodeAndSellerCode);
    } catch (JsonProcessingException e) {
      log.error("Error caught while processing internal bulk upload for "
              + "internalProcessDataRequestId {} ",
          bulkInternalProcessData.getInternalProcessRequestId(), e);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  private void processBulkApprovalRejectionRequests(String storeId,
    Page<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
        bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
          .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);
    Optional.ofNullable(bulkInternalProcessDataList).orElse(new ArrayList<>()).stream().forEach(
      bulkInternalProcessData -> publishBulkApprovalRejectionAndUpdateStatus(storeId,
        bulkInternalProcessData, username));
  }

  private void processBulkMasterSkuReviewRequests(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name(), fetchBatchSize);
    Optional.ofNullable(bulkInternalProcessDataList).orElse(new ArrayList<>()).forEach(
        bulkInternalProcessData -> publishBulkMasterSkuReviewEventsAndUpdateStatus(storeId, bulkInternalProcessData,
            username));
  }

  private void processBulkAssigneeRequests(String storeId,
    Page<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
        bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
          .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);
    Optional.ofNullable(bulkInternalProcessDataList).orElse(new ArrayList<>()).stream().forEach(
      bulkInternalProcessData -> publishBulkAssigneeAndUpdateStatus(storeId,
        bulkInternalProcessData, username));
  }

  private void processAutoApprovedProductBulkAssignRequests(String storeId,
      Page<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
                .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);
    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      bulkInternalProcessDataList.forEach(
          bulkInternalProcessData -> publishAutoApprovedProductsBulkAssign(storeId,
              bulkInternalProcessData, username));
    }
  }

  private void processBulkIPRProductAddReview(String storeId,
      Page<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId)
                .collect(Collectors.toList()), ProcessStatus.PENDING.name(), fetchBatchSize);

    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      bulkInternalProcessDataList.forEach(
          bulkInternalProcessData -> publishBulkIPRProductUpload(storeId, bulkInternalProcessData,
              username));
    }
  }

  private void publishBulkIPRProductUpload(String storeId, BulkInternalProcessData bulkInternalProcessData,
      String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer
        .send(kafkaTopicProperties.getBulkIprPortalAddReviewProcessEvent(), bulkInternalProcessData.getInternalProcessRequestId(),
            RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void processBulkRebateUpload(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize, String username) {
    List<BulkInternalProcessData> bulkInternalProcessDataList = internalProcessService
        .getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            bulkInternalProcesses.getContent().stream().map(BulkInternalProcess::getId).collect(Collectors.toList()),
            ProcessStatus.PENDING.name(), fetchBatchSize);

    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      bulkInternalProcessDataList
          .forEach(bulkInternalProcessData -> publishBulkRebateUpload(storeId, bulkInternalProcessData, username));
    }
  }

  private void publishBulkRebateUpload(String storeId, BulkInternalProcessData bulkInternalProcessData,
      String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer
        .send(kafkaTopicProperties.getBulkPriceRebateUpload(), bulkInternalProcessData.getInternalProcessRequestId(),
            RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void processBulkPriceUpdateRequests(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize, String username, String topic) {
    Map<String, String> internalProcessRequestIdCreatedByMap = bulkInternalProcesses.getContent()
        .stream()
        .collect(Collectors.toMap(BulkInternalProcess::getId, BulkInternalProcess::getCreatedBy));
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            new ArrayList<>(internalProcessRequestIdCreatedByMap.keySet()),
            ProcessStatus.PENDING.name(), fetchBatchSize);
    Map<String, List<BulkInternalProcessData>> bulkInternalProcessGroupedData = new HashMap<>();
    bulkInternalProcessDataList.forEach(bulkInternalProcessData -> {
      bulkInternalProcessGroupedData.computeIfAbsent(
          bulkInternalProcessData.getParentCode() + bulkInternalProcessData.getInternalProcessRequestCode(),
          k -> new ArrayList<>()).add(bulkInternalProcessData);
    });
    int maxNumberOfL5sInUpdateRequest = getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE);
    for (Map.Entry<String, List<BulkInternalProcessData>> groupedData : bulkInternalProcessGroupedData.entrySet()) {
      List<List<BulkInternalProcessData>> partition =
          Lists.partition(groupedData.getValue(), maxNumberOfL5sInUpdateRequest);
      partition.forEach(bulkInternalProcessData -> publishBulkPriceUpdate(storeId,
          bulkInternalProcessData, StringUtils.isEmpty(username) ?
              internalProcessRequestIdCreatedByMap.get(bulkInternalProcessData.get(0).getInternalProcessRequestId()) : username,
          groupedData.getKey(), topic));
    }
  }

  private void processBulkSkuLevelRebateUpload(String storeId, Page<BulkInternalProcess> bulkInternalProcesses,
      int fetchBatchSize, String username) {
    Map<String, String> internalProcessRequestIdCreatedByMap = bulkInternalProcesses.getContent()
        .stream()
        .collect(Collectors.toMap(BulkInternalProcess::getId, BulkInternalProcess::getCreatedBy));
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(storeId,
            new ArrayList<>(internalProcessRequestIdCreatedByMap.keySet()),
            ProcessStatus.PENDING.name(), fetchBatchSize);

    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      bulkInternalProcessDataList.forEach(bulkInternalProcessData -> publishBulkSkuLevelRebateUpload(storeId,
          bulkInternalProcessData, internalProcessRequestIdCreatedByMap.get(bulkInternalProcessData.getInternalProcessRequestId())));
    }
  }

  private void publishBulkSkuLevelRebateUpload(String storeId, BulkInternalProcessData bulkInternalProcessData,
      String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer
        .send(kafkaTopicProperties.getBulkSkuLevelRebateUpload(), bulkInternalProcessData.getInternalProcessRequestId(),
            RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  @Override
  public void processEvent(InternalProcessDataDomainEventModel storeCopyProductCreationDetails) throws Exception {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            storeCopyProductCreationDetails.getParentCode(), storeCopyProductCreationDetails.getProcessType(),
            storeCopyProductCreationDetails.getInternalProcessRequestId(), BulkProcessData.STATUS_IN_PROGRESS);
    if (CollectionUtils.isEmpty(bulkInternalProcessDataList)) {
      log.warn("No rows found for request : {}", storeCopyProductCreationDetails);
      return;
    }
    List<LinkedHashMap<String, Object>> userInputRows = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
      bulkInternalProcessData.setStatus(BulkProcessData.STATUS_PROCESSING);
      LinkedHashMap<String, Object> row = RequestHelper.getInputRows(bulkInternalProcessData);
      userInputRows.add(row);
    }
    bulkInternalProcessDataList = internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    String newProductCode = null;
    try {
      String productCode = userInputRows.get(0).get(StoreCopyConstants.FIELD_PRODUCT_CODE).toString();
      String productSku = userInputRows.get(0).get(StoreCopyConstants.FIELD_PRODUCT_SKU).toString();
      Set<String> requestItemSkus =
          userInputRows.stream().map(row -> row.get(StoreCopyConstants.FIELD_ITEM_SKU).toString())
              .collect(Collectors.toSet());
      List<ProductAndItemInfoResponseV2> productAndItemInfoResponseV2List = null;
      ProductAndItemInfoResponseV2 firstProductAndItemInfoResponseV2 = null;
      try {
        GetProductInfoRequestV2 request = new GetProductInfoRequestV2();
        request.setItemSkus(requestItemSkus);
        GdnRestListResponse<ProductAndItemInfoResponseV2> response =
            xProductOutboundService.getProductInfoByItemSku(request);
        productAndItemInfoResponseV2List = response.getContent();
        firstProductAndItemInfoResponseV2 = response.getContent().stream().findFirst().get();
      } catch (Exception e) {
        log.error("Error while fetching the details from x-product for productSku : {} ", productSku, e);
        updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, X_PRODUCT_ERROR_MESSAGE, null);
        return;
      }
      if (firstProductAndItemInfoResponseV2.getProduct().isSuspended()) {
        updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, SUSPENDED_ERROR_MESSAGE, null);
        return;
      }
      if (firstProductAndItemInfoResponseV2.getProduct().isForceReview()) {
        updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, TAKEN_DOWN_ERROR_MESSAGE, null);
        return;
      }
      if (firstProductAndItemInfoResponseV2.getProduct().isMarkForDelete()) {
        updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, DELETED_ERROR_MESSAGE, null);
        return;
      }

      for (ProductAndItemInfoResponseV2 productAndItemInfoResponseV2 : productAndItemInfoResponseV2List) {
        if (productAndItemInfoResponseV2.getItem().isArchived()) {
          updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, ARCHIVED_ERROR_MESSAGE, null);
          return;
        }
      }

      Map<String, LinkedHashMap<String, Object>> requestItems = new HashMap<>();
      for (LinkedHashMap<String, Object> row : userInputRows) {
        String itemSku = row.get(StoreCopyConstants.FIELD_ITEM_SKU).toString();
        requestItems.put(itemSku, row);
      }

      ProductDetailResponse response = pcbOutboundService.getProductDetailByProductCode(productCode, false, false);
      CategoryDetailResponse category =
          this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(Constant.STORE_ID,
              response.getCategoryCodes().get(response.getCategoryCodes().size() - 1));

      //fix this to get only attribute details and only relevant values not all values;
      Map<String, AttributeResponse> attributes = this.getAttributes(Constant.STORE_ID, category, response);

      ProfileResponse businessPartner =
          this.businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID,
              bulkInternalProcessDataList.get(0).getSellerCode());

      ProductCreationRequest productCreationRequest =
          BulkCreationCommonUtil.toProductCreationRequestForCopyStore(attributes, category,
              businessPartner, response, firstProductAndItemInfoResponseV2,
              productAndItemInfoResponseV2List, requestItems, bpBopisRestrictionEnabled,
              valueTypeAdditionForDefiningAttributes);

      boolean isProtectedBrandAuthenticated =
        checkProtectedBrandCreationAuthorisation(productCreationRequest.getStoreId(),
          productCreationRequest.getBrandCode(), productCreationRequest.getBusinessPartnerCode());

      if (!isProtectedBrandAuthenticated) {
        updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, PROTECTED_BRAND_MESSAGE, null);
        return;
      }

      if (CollectionUtils.isNotEmpty(firstProductAndItemInfoResponseV2.getProduct().getProductSpecialAttributes())) {
        BulkCreationCommonUtil.generateProductBusinessPartnerRequest(
            firstProductAndItemInfoResponseV2.getProduct().getProductSpecialAttributes(), category,
            productCreationRequest);
      }

      newProductCode =
          String.valueOf(productRepository.generateProductCode(Constant.REQUEST_ID, Constant.USER_NAME).getValue());
      BulkInternalProcess bulkInternalProcess =
          internalProcessService.findByInternalProcessRequestCode(storeCopyProductCreationDetails.getStoreId(),
              bulkInternalProcessDataList.get(0).getInternalProcessRequestCode());
      productCreationRequest.setProductCode(newProductCode);
      productCreationRequest.setCreatedBy(bulkInternalProcess.getCreatedBy());
      productCreationRequest.setCreatedDate(new Date());
      log.info("Product Creation request for product code : {} request : {}", newProductCode, productCreationRequest);
      this.productRepository.createProduct(bulkInternalProcessDataList.get(0).getInternalProcessRequestCode(), Constant.SYSTEM,
          productCreationRequest);
      updateState(bulkInternalProcessDataList, ProcessStatus.COMPLETED, null, newProductCode);
    } catch (Exception e) {
      log.error("Error while coping product for request : {} , generatedProductCode : {}",
          storeCopyProductCreationDetails, newProductCode, e);
      updateState(bulkInternalProcessDataList, ProcessStatus.FAILED, EXCEPTION_ERROR_MESSAGE + e.getMessage(),
          newProductCode);
    }
  }

  private boolean checkProtectedBrandCreationAuthorisation(String storeId, String brandCode,
    String businessPartnerCode) {
    String channelId = MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    String clientId = MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    String requestId = MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    String username = MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    SimpleBooleanResponse simpleBooleanResponse = pcbOutboundService
      .getBrandAuthorisation(storeId, channelId, clientId, requestId, username, businessPartnerCode,
        brandCode);
    return simpleBooleanResponse.getResult();
  }

  private Map<String, AttributeResponse> getAttributes(String storeId, CategoryDetailResponse category,
      ProductDetailResponse productDetailResponse) throws Exception {
    List<String> attributesCodeInCategory =
        category.getCategoryAttributes().stream().map(CategoryAttributeResponse::getAttribute)
            .map(AttributeResponse::getAttributeCode).distinct().collect(Collectors.toList());

    Map<String, ProductAttributeResponse> existingAttributes =
        getExistingProductAttributeMap(productDetailResponse);

    List<AttributeResponse> attributeResponses =
        productAttributeRepository.getAttributeDetailByAttributeCodes(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, attributesCodeInCategory, true);

    Map<String, AttributeResponse> attributeResponseMap = attributeResponses.stream().collect(
        Collectors.toMap(AttributeResponse::getAttributeCode, Function.identity(), (value1, value2) -> value2));

    Map<String, String> attributeIdAndValueMap =
        BulkCreationCommonUtil.getAttributeIdAndValuesMap(attributeResponses, existingAttributes);

    getAttributeValuesFromPCB(storeId, attributeIdAndValueMap, attributeResponseMap);

    setAttributeAllowedAndPredefinedValues(attributeResponses);

    return attributeResponseMap;
  }

  private Map<String, ProductAttributeResponse> getExistingProductAttributeMap(
      ProductDetailResponse productDetailResponse) {
    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : Optional.ofNullable(
        productDetailResponse.getProductAttributeResponses()).orElse(new ArrayList<>())) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }
    return existingAttributes;
  }

  private void getAttributeValuesFromPCB(String storeId, Map<String, String> attributeIdAndValueMap,
      Map<String, AttributeResponse> attributeResponseMap) {
    for (Map.Entry<String, String> attributeIdAndValueEntry : attributeIdAndValueMap.entrySet()) {
      AttributeResponse attributeResponseWithValue =
          pcbOutboundService.getAttributeDetailById(storeId, attributeIdAndValueEntry.getKey(),
              attributeIdAndValueEntry.getValue());
      AttributeResponse attributeResponse = attributeResponseMap.get(attributeResponseWithValue.getAttributeCode());
      attributeResponse.setPredefinedAllowedAttributeValues(attributeResponse.getPredefinedAllowedAttributeValues());
    }
  }

  private static void setAttributeAllowedAndPredefinedValues(List<AttributeResponse> attributeResponses) {
    for (AttributeResponse attributeResponse : attributeResponses) {
      attributeResponse.setAllowedAttributeValues(
          Optional.ofNullable(attributeResponse.getAllowedAttributeValues()).orElse(new ArrayList<>()));
      attributeResponse.setPredefinedAllowedAttributeValues(
          Optional.ofNullable(attributeResponse.getPredefinedAllowedAttributeValues()).orElse(new ArrayList<>()));
    }
  }

  private List<BulkInternalProcessData> updateState(List<BulkInternalProcessData> updatedList, ProcessStatus status,
      String errorMessage, String notes) {
    for (BulkInternalProcessData data : updatedList) {
      data.setStatus(status.name());
      data.setErrorMessage(errorMessage);
      if (StringUtils.isNotEmpty(notes)) {
        data.setNotes(notes);
      }
    }
    return internalProcessService.saveInternalProcessData(updatedList);
  }

  @Override
  public void processInternalBulkUploadEvent(String storeId,
      String updatedBy, String processType, String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.getBulkInternalProcessDataById(storeId, internalProcessDataRequestId);
    try {
      InternalBulkUploadRequest internalBulkUploadRequest =
          RequestHelper.toInternalBulkUploadRequestFromJson(bulkInternalProcessData.getData());
      SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest =
          RequestHelper.toSimpleMasterProductUpdateRequest(internalBulkUploadRequest);
      BulkMasterProductUpdateRequest bulkMasterProductUpdateRequest =
          RequestHelper.toBulkMasterProductUpdateRequest(simpleMasterProductUpdateRequest, storeId, updatedBy);
      productRepository.updateMasterProducts(bulkMasterProductUpdateRequest);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.COMPLETED.name(), StringUtils.EMPTY);
    } catch (Exception e) {
      log.error("Error caught while processing internal bulk upload for internalProcessDataRequestId {} ",
          internalProcessDataRequestId, e);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      this.trackerService.sendTracker(TrackerConstants.MASTER_PRODUCT_BULK_UPDATE,
          TrackerConstants.MASTER_PRODUCT_UPDATE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, updatedBy);
    }
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
  }

  @Override
  public void processUpdateSalesCategoryEvent(String storeId, String processType, String parentCode,
      String internalProcessRequestId) {
    List<BulkInternalProcessData> bulkInternalProcessDataList = internalProcessService
        .getAllProductByParentCodeAndProcessTypeAndStatusAndInternalProcessRequestId(parentCode,
          processType, ProcessStatus.IN_PROGRESS.name(), internalProcessRequestId);
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setStatus(
        ProcessStatus.PROCESSING.name()));
    List<BulkInternalProcessData> updateBulkInternalProcessDataList =
      internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    for (BulkInternalProcessData bulkInternalProcessData : updateBulkInternalProcessDataList) {
      try {
        updateProductSalesCategory(storeId, bulkInternalProcessData);
      } catch (Exception e) {
        log.info("Error caught while processing sales category update. ", e);
        RequestHelper
            .updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
                Constant.SYSTEM_ERROR);
      }
    }
    internalProcessService.saveInternalProcessData(updateBulkInternalProcessDataList);
  }

  @Override
  public void processDeleteBrandAuthorisationEvent(String storeId, String processType, String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData = internalProcessService
            .bulkInternalProcessDataByIdAndStatus(storeId, internalProcessDataRequestId, ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData =
            internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)).get(0);
        BrandAuthorisationRequest brandAuthorisationRequest =
            RequestHelper.toBrandAuthorisationRequestFromJson(bulkInternalProcessData.getData());
        pcbOutboundService.deleteBrandAuthorisation(storeId, brandAuthorisationRequest.getSellerCode(),
            brandAuthorisationRequest.getBrandCode());
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.COMPLETED.name(),
            StringUtils.EMPTY);
      } catch (Exception e) {
        log.error("Exception while processing delete brand authorization for internalProcessDataRequestId : {} ",
            internalProcessDataRequestId, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  private void updateProductSalesCategory(String storeId, BulkInternalProcessData bulkInternalProcessData)
      throws IOException {
    SalesCategoryUpdateRequest salesCategoryUpdateRequest =
        RequestHelper.toSalesCategoryUpdateRequestFromJson(bulkInternalProcessData.getData());
    CategoryResponse categoryResponse =
        pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(salesCategoryUpdateRequest.getCnCategoryCode());
    if (Objects.nonNull(categoryResponse) && Constant.SALES_CATALOG.equals(
        categoryResponse.getCatalog().getCatalogType())) {
      if (Constant.ONE_STRING.equals(salesCategoryUpdateRequest.getOperationType()))
        addSalesCategory(storeId, bulkInternalProcessData, salesCategoryUpdateRequest);
      else if (Constant.ZERO_STRING.equals(salesCategoryUpdateRequest.getOperationType())) {
        deleteSalesCategory(storeId, bulkInternalProcessData, salesCategoryUpdateRequest);
      } else {
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), SalesCategoryUpdateConstants.INVALID_OPERATION_TYPE);
      }
    } else {
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
          Constant.INVALID_CATEGORY);
    }
  }

  private void deleteSalesCategory(String storeId, BulkInternalProcessData bulkInternalProcessData,
      SalesCategoryUpdateRequest salesCategoryUpdateRequest) {
    xProductOutboundService.deleteSalesCategory(storeId, salesCatalogCode, salesCategoryUpdateRequest.getCnCategoryCode(),
        Arrays.asList(bulkInternalProcessData.getParentCode()));
    RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.COMPLETED.name(),
        StringUtils.EMPTY);
  }

  private void addSalesCategory(String storeId, BulkInternalProcessData bulkInternalProcessData,
      SalesCategoryUpdateRequest salesCategoryUpdateRequest) {
    xProductOutboundService.addSalesCategory(storeId, salesCatalogCode, salesCategoryUpdateRequest.getCnCategoryCode(),
        Arrays.asList(bulkInternalProcessData.getParentCode()));
    RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.COMPLETED.name(),
        StringUtils.EMPTY);
  }

  @Async
  @Override
  public void abortPendingBulkInternalProcessBefore(String storeId, String processType) {
    Integer abortTimeInMinute = getAbortMinuesBeforeByProcessType(storeId, processType);
    Date pendingToAbortDate = DateUtils.addMinutes(new Date(), -abortTimeInMinute); //abortTimeInMinute will be negative
    log.info("Aborting all pending bulk internal process tasks for storeId {} before {}", storeId, pendingToAbortDate);
    internalProcessService.abortPendingBulkInternalProcess(storeId, pendingToAbortDate, processType);
  }

  @Async
  @Override
  public void failPendingBulkInternalProcessDataBefore(String storeId, String processType) {
    Integer failTimeInMinutes = getFailMinuesBeforeByProcessType(storeId, processType);
    Date pendingToAbortDate = DateUtils.addMinutes(new Date(), -failTimeInMinutes); //failTimeInMinutes will be negative
    log.info("Fail all published bulk internal process data for storeId {} before {}", storeId, pendingToAbortDate);
    internalProcessService.failPendingBulkInternalProcessData(storeId, pendingToAbortDate, processType);
  }

  @Override
  @Async
  public void deleteOldBulkInternalProcessRequest(String storeId, String processType) {
    int deleteBatchSize = getDeleteBatchSizeByProcessType(storeId, processType);
    int deleteDaysBefore = getDeleteDaysBeforeByProcessType(storeId, processType);
    Date date = DateUtils.addDays(new Date(), -deleteDaysBefore); // deleteDaysBefore will be a negative value

    int page = 0;
    Page<BulkInternalProcess> bulkInternalProcesses;
    do {
      Pageable pageable = PageRequest.of(page, deleteBatchSize,
          Sort.by(Sort.Direction.ASC, Constant.CREATED_DATE).and(Sort.by(Sort.Direction.ASC, Constant.ID)));
      bulkInternalProcesses = internalProcessService
          .getBulkInternalProcessByProcessTypeAndStatusAndCreatedDate(storeId, processType, date, DELETE_STATUS,
              pageable);
      if (CollectionUtils.isNotEmpty(bulkInternalProcesses.getContent())) {
        for (BulkInternalProcess bulkInternalProcess : bulkInternalProcesses.getContent()) {
          ProcessorUtils.deleteInternalProcessDirectoryByProcessTypeAndRequestCode(processType,
              bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getFileName());
          internalProcessService.deleteBulkInternalProcessDataByInternalRequestId(bulkInternalProcess.getId());
        }
      }
      page++;
    } while (page * deleteBatchSize < bulkInternalProcesses.getTotalElements());
  }

  public void uploadBulkInternalProcess(String storeId, BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest) {
    if (Boolean.FALSE.equals(fileStorageServiceBean.isFileExists(
      BulkInternalUploadRequestDTO.builder().internalProcessRequestCode(
          bulkInternalProcessUploadRequest.getInternalProcessRequestCode()).bulkInternalProcessType(
          Enum.valueOf(BulkInternalProcessType.class,
            bulkInternalProcessUploadRequest.getProcessType())).fileName(
          Optional.ofNullable(bulkInternalProcessUploadRequest)
            .map(BulkInternalProcessUploadRequest::getFileName).orElse(StringUtils.EMPTY))
          .createdBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER))
        .build()))) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND);
    }
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcess(storeId, bulkInternalProcessUploadRequest);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.PENDING.name());
  }

  private void publishStoreCopyProductAndUpdateStatus(
      List<BulkInternalProcessPendingDataDTO> bulkInternalProcessDataParentCodeList, String storeId) {
    for (BulkInternalProcessPendingDataDTO processPendingDataDTO : bulkInternalProcessDataParentCodeList) {
      updateInternalProcessDataStatus(processPendingDataDTO);
      log.info("Publish product create for product code : {}, productType : {}, id : {}", processPendingDataDTO.getParentCode(),
          processPendingDataDTO.getProcessType(), processPendingDataDTO.getInternalProcessRequestId());
      kafkaProducer.send(kafkaTopicProperties.getStoreCopyProductCreationDetails(),
          RequestHelper.toInternalProcessDataDomainEventModel(processPendingDataDTO, storeId));
    }
  }

  private void publishUpdateSalesCategoryAndUpdateStatus(String storeId,
      BulkInternalProcessData bulkInternalProcessData) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getUpdateSalesCategoryDetails(),
        RequestHelper.toInternalProcessDataDomainEventModel(bulkInternalProcessData, storeId));
  }

  private void publishDeleteBrandAuthorisationAndUpdateStatus(String storeId, BulkInternalProcessData bulkInternalProcessData) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getDeleteBrandAuthorisation(), RequestHelper.toInternalProcessDataDomainEventModel(bulkInternalProcessData, storeId));
  }

  private void publishBulkBrandAuthorisationAndUpdateStatus(String storeId,
      BulkInternalProcessData bulkInternalProcessData, String username, String brandCodeAndSellerCode) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getBrandAuthorisationBulkProcess(), brandCodeAndSellerCode,
        RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void publishBulkApprovalRejectionAndUpdateStatus(String storeId,
    BulkInternalProcessData bulkInternalProcessData, String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getBulkApprovalRejectionProcessEvent(),
      RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void publishBulkMasterSkuReviewEventsAndUpdateStatus(String storeId,
      BulkInternalProcessData bulkInternalProcessData, String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getBulkMasterSkuReviewProcessEvent(),
        bulkInternalProcessData.getParentCode(),
        RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void publishBulkAssigneeAndUpdateStatus(String storeId,
    BulkInternalProcessData bulkInternalProcessData, String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getBulkAssigneeMasterSkuProcessEvent(),
      bulkInternalProcessData.getInternalProcessRequestId(),
      RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId,
        username));
  }

  private void publishAutoApprovedProductsBulkAssign(String storeId,
      BulkInternalProcessData bulkInternalProcessData, String username) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getAutoApprovedProductsBulkAssignProcessEvent(),
        bulkInternalProcessData.getInternalProcessRequestId(),
        RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId,
            username));
  }

  private void publishBulkPriceUpdate(String storeId, List<BulkInternalProcessData> bulkInternalProcessDataList,
      String username, String domainKey, String topic) {
    bulkInternalProcessDataList.forEach(
        bulkInternalProcessData -> bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name()));
    internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
        RequestHelper.toInternalBulkUploadDataDomainEventModel(
            bulkInternalProcessDataList.stream().map(BulkInternalProcessData::getId).collect(Collectors.toList()),
            bulkInternalProcessDataList.get(0).getProcessType(), storeId, username);
    log.info("Publishing event for : {} message : {}", topic, internalBulkUploadDataDomainEventModel);
    kafkaProducer.send(topic, domainKey, internalBulkUploadDataDomainEventModel);
  }

  private void publishInternalBulkUploadAndUpdateStatus(String storeId, String username, BulkInternalProcessData bulkInternalProcessData) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getInternalBulkUploadDetails(), RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId, username));
  }

  private void publishVendorBulkAssignmentAndUpdateStatus(String storeId, BulkInternalProcessData bulkInternalProcessData) {
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    kafkaProducer.send(kafkaTopicProperties.getBulkVendorAssignmentEvent(),
      RequestHelper.toInternalBulkUploadDataDomainEventModel(bulkInternalProcessData, storeId,
        bulkInternalProcessData.getUpdatedBy()));
  }

  private List<BulkInternalProcessData> getInternalProcessDataFromExcel(String storeId,
      BulkInternalProcess bulkInternalProcess, String userName) throws Exception {
    Sheet excelSheetData = fileStorageServiceBean.getFileDataWithInternalUploadRequest(
      BulkInternalUploadRequestDTO.builder().bulkInternalProcessType(
        Enum.valueOf(BulkInternalProcessType.class, bulkInternalProcess.getProcessType()))
        .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
        .fileName(bulkInternalProcess.getFileName()).relativePath(bulkInternalProcess.getFileName())
        .createdBy(bulkInternalProcess.getCreatedBy())
        .build());
    POIUtil.validateNumberOfRows(excelSheetData, bulkInternalProcess.getInternalProcessRequestCode(),
        bulkMaxNumberOfRows, validateBulkMaxNumberOfRows);
    int headerRowId=0;
    int startFrom = 1;
    if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      headerRowId = 2;
      startFrom = 2;
    }
    List<Map<String, String>> internalProcessDataFromExcel;
    if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())) {
      internalProcessDataFromExcel = POIUtil.readFromExcelFromSpecificHeadersForBulkPriceUpdate(excelSheetData,
          6, 5, BulkParameters.BULK_PRICE_UPDATE_NEW_UPLOAD_HEADERS);
    } else {
      internalProcessDataFromExcel =
          POIUtil.readStringValueFromExcelForBulkUpdate(excelSheetData, startFrom, headerRowId, 0
            , bulkInternalProcess.getProcessType());
    }
    if (!BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())
        || CollectionUtils.isNotEmpty(internalProcessDataFromExcel)) {
      if (RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess)) {
        log.error("Failed to validate header for internal-process-request-code : {} ",
            bulkInternalProcess.getInternalProcessRequestCode());
        throw new ApplicationException(ErrorCategory.INVALID_FORMAT, bulkInternalProcess.getNotes());
      }
    }
    log.info("File data read for internal-process-request-code : {} , number of rows : {} ",
        bulkInternalProcess.getInternalProcessRequestCode(), internalProcessDataFromExcel.size());
    Map<String, List<String>> reviewers = null;
    if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
        .equals(bulkInternalProcess.getProcessType())) {
      reviewers = getReviewers();
    }
    BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO =
        RequestHelper.getBulkInternalProcessDataGenerationDTO(brandAuthEndYear, bulkPriceUpdateMaxRows,
            bulkRebateMaxRows, bulkProductTypeTaggingMaxRows, bulkSkuLevelRebateMaxRows, bulkPriceUpdateNewMaxRows,
            storeId, userName, internalProcessDataFromExcel, reviewers, bulkInternalProcess);
    if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      String iprValidActions = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.IPR_ACTIONS).getValue();
      String iprSource = systemParameterConfigService.findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.IPR_SOURCE)
          .getValue();
      String iprViolationTypes = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.IPR_VIOLATION_TYPES).getValue();
      String iprReasons = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.IPR_REASONS).getValue();
      Map<String, List<String>> iprReviewers = getIprReviewers();
      bulkInternalProcessDataGenerationDTO.setIprValidActions(
          Arrays.stream(iprValidActions.split(Constant.COMMA)).map(String::trim)
              .collect(Collectors.toSet()));
      bulkInternalProcessDataGenerationDTO.setIprSource(
          Arrays.stream(iprSource.split(Constant.COMMA)).map(String::trim)
              .collect(Collectors.toSet()));
      bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
          Arrays.stream(iprViolationTypes.split(Constant.COMMA)).map(String::trim)
              .collect(Collectors.toSet()));
      bulkInternalProcessDataGenerationDTO.setIprReasons(
          Arrays.stream(iprReasons.split(Constant.COMMA)).map(String::trim)
              .collect(Collectors.toSet()));
      bulkInternalProcessDataGenerationDTO.setReviewers(iprReviewers);
    }
    List<BulkInternalProcessData> bulkInternalProcessData =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessData =
          performResultantActionForRestrictedKeywordUpload(bulkInternalProcessData, bulkInternalProcess, storeId, userName);
    }
    if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessData = performResultantActionForBrandAuthUpload(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.BULK_APPROVAL.name().equals(bulkInternalProcess.getProcessType())
            || BulkInternalProcessType.BULK_REJECTION.name().equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessData = performResultantActionForBulkApproval(bulkInternalProcessData, bulkInternalProcess,
              internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name()
      .equals(bulkInternalProcess.getProcessType())) {
      bulkInternalProcessData =
        performResultantActionForBrandAuthUpload(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBrandAuthUpload(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBrandAuthUpload(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (Constant.INTERNAL_BULK_UPLOAD.equals(bulkInternalProcess.getProcessType())) {
      addToTracker(bulkInternalProcessData, bulkInternalProcess.getUpdatedBy());
    }
    if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBulkPriceUpdate(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (
      BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType())
        || BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name()
        .equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBulkPriceRebateAndProductTagging(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name()
        .equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBrandAuthUpload(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForBulkSkuLevelRebate(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel);
    }
    if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(bulkInternalProcess.getProcessType())) {
      performResultantActionForNewBulkPriceUpdate(bulkInternalProcessData, bulkInternalProcess,
          internalProcessDataFromExcel, bulkInternalProcessDataGenerationDTO.isNoRowsDetectedForPriceUpdate());
    }
    return bulkInternalProcessData;
  }


  private List<BulkInternalProcessData> performResultantActionForBrandAuthUpload(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel) {
    List<BulkInternalProcessData> failedBulkInternalProcessData = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .collect(Collectors.toList());
    if (internalProcessDataFromExcel.size() == failedBulkInternalProcessData.size()) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
    return bulkInternalProcessDataList;
  }

  private List<BulkInternalProcessData> performResultantActionForBulkPriceRebateAndProductTagging(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel) throws Exception {
    boolean isBulkPriceRebate = BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(bulkInternalProcess.getProcessType());
    String templateId = isBulkPriceRebate ?
      EmailConstants.BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_ID :
      EmailConstants.BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_ID;
    String emailSubject = isBulkPriceRebate ?
      EmailConstants.BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE :
      EmailConstants.BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_TEMPLATE;
    if (bulkInternalProcessDataList.isEmpty()) {
      sendEmailForEmptyData(bulkInternalProcess, internalProcessDataFromExcel);
      return Collections.emptyList();
    }
    List<BulkInternalProcessData> failedBulkInternalProcessData = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .collect(Collectors.toList());
    if (internalProcessDataFromExcel.size() == failedBulkInternalProcessData.size()) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
    return bulkInternalProcessDataList;
  }

  private void sendEmailForEmptyData(BulkInternalProcess bulkInternalProcess,
    List<Map<String, String>> internalProcessDataFromExcel) throws Exception {
      updateRebateInternalProcessForEmptyData(bulkInternalProcess, internalProcessDataFromExcel);
  }

  private void updateRebateInternalProcessForEmptyData(
    BulkInternalProcess bulkInternalProcess, List<Map<String, String>> internalProcessDataFromExcel)
    throws Exception {
    bulkInternalProcess.setNotes(
        ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN.concat(String.valueOf(bulkRebateMaxRows)));
    bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
    bulkRebateUpdateService.sendEmailNotification(EmailConstants.BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_ID,
        EmailConstants.BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE, bulkInternalProcess.getFileName(),
        bulkInternalProcess.getCreatedBy(), null, null, bulkRebateMaxRows);
  }

  private List<BulkInternalProcessData> performResultantActionForBulkPriceUpdate(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel) throws Exception {
    if (bulkInternalProcessDataList.size() == 0) {
      bulkInternalProcess.setNotes(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN);
      bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      bulkPriceUpdateService.sendEmailNotification(EmailConstants.BULK_PRICE_UPDATE_MAX_ROWS_FAILURE_ID,
          EmailConstants.BULK_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE,
          bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
          String.valueOf(internalProcessDataFromExcel.size()), String.valueOf(bulkPriceUpdateMaxRows));
      return new ArrayList<>();
    }
    List<BulkInternalProcessData> failedBulkInternalProcessData = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .collect(Collectors.toList());
    if (internalProcessDataFromExcel.size() == failedBulkInternalProcessData.size()) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
    return bulkInternalProcessDataList;
  }

  private List<BulkInternalProcessData> performResultantActionForBulkApproval(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel) {
    int failedDataRows = bulkInternalProcessDataList.stream().filter(bulkInternalProcessData -> bulkInternalProcessData.getStatus().equalsIgnoreCase(ProcessStatus.FAILED.name()))
            .mapToInt(data -> 1).sum();
    if (internalProcessDataFromExcel.size() == failedDataRows) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
    return bulkInternalProcessDataList;
  }

  private List<BulkInternalProcessData> performResultantActionForRestrictedKeywordUpload(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      String storeId, String username) throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataSuccess = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.PENDING.name().equals(bulkInternalProcessData.getStatus()))
        .collect(Collectors.toList());
    List<BulkInternalProcessData> bulkInternalProcessDataFailed = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .collect(Collectors.toList());
    if (bulkInternalProcessDataFailed.size() == bulkInternalProcess.getTotalCount()) {
      //If all internal process data are failed , status is updated to published so scheduler 3 directly picks up.
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
      return bulkInternalProcessDataFailed;
    } else {
      Set<String> categoryCodeListFromPCB = new HashSet<>();
      List<BulkInternalProcessData> bulkInternalProcessDataListToSave = new ArrayList<>();
      for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataSuccess) {
        //The value of "Applicable For All Categories"/"Delete for all Categories" is stored in notes.
        //If YES, call pcb and get all master categories.
        if (ExcelHeaderNames.YES.equals(bulkInternalProcessData.getNotes())) {
          performRestrictedKeywordUploadForAllCategory(categoryCodeListFromPCB, bulkInternalProcessDataListToSave,
              bulkInternalProcessData, storeId, username);
        } else {
          //If NO populate data table based on categories mentioned in "Applicable to these categories".
          performRestrictedKeywordUploadForApplicableCategory(bulkInternalProcessDataListToSave,
              bulkInternalProcessData);
        }
      }
      bulkInternalProcessDataListToSave.addAll(bulkInternalProcessDataFailed);
      return bulkInternalProcessDataListToSave;
    }
  }

  private void performRestrictedKeywordUploadForApplicableCategory(
      List<BulkInternalProcessData> bulkInternalProcessDataListToSave, BulkInternalProcessData bulkInternalProcessData)
      throws JsonProcessingException {
    RestrictedKeywordRequestData restrictedKeywordRequestData =
        objectMapper.readValue(bulkInternalProcessData.getData(), RestrictedKeywordRequestData.class);
    Set<String> categoryCodes = restrictedKeywordRequestData.getApplicableCategoryList();
    categoryCodes.forEach(categoryCode -> {
      BulkInternalProcessData bulkInternalProcessDataNew = new BulkInternalProcessData();
      BeanUtils.copyProperties(bulkInternalProcessData, bulkInternalProcessDataNew, "notes");
      bulkInternalProcessDataNew.setParentCode(categoryCode);
      bulkInternalProcessDataListToSave.add(bulkInternalProcessDataNew);
    });
  }

  private void performRestrictedKeywordUploadForAllCategory(Set<String> categoryCodeListFromPCB,
      List<BulkInternalProcessData> bulkInternalProcessDataListToSave, BulkInternalProcessData bulkInternalProcessData,
      String storeId, String username) throws JsonProcessingException {
    if (CollectionUtils.isEmpty(categoryCodeListFromPCB)) {
      List<CategoryDTO> categoryDTOS =
          pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(storeId, 0, childCategoryMaxSize, FILTER_TYPE,
              DOCUMENT_FILTER_TYPE, username);
      categoryDTOS.forEach(categoryDTO -> categoryCodeListFromPCB.add(categoryDTO.getCategoryCode()));
    }
    categoryCodeListFromPCB.forEach(categoryCode -> {
      BulkInternalProcessData bulkInternalProcessDataNew = new BulkInternalProcessData();
      BeanUtils.copyProperties(bulkInternalProcessData, bulkInternalProcessDataNew, "notes");
      bulkInternalProcessDataNew.setParentCode(categoryCode);
      bulkInternalProcessDataListToSave.add(bulkInternalProcessDataNew);
    });
  }

  private void performResultantActionForBulkSkuLevelRebate(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel) throws Exception {
    if (bulkInternalProcessDataList.isEmpty()) {
      bulkInternalProcess.setNotes(ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN + bulkSkuLevelRebateMaxRows);
      bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      bulkSkuLevelRebateService.sendEmailNotification(EmailConstants.BULK_SKU_LEVEL_REBATE_MAX_ROWS_FAILURE_TEMPLATE_ID,
          EmailConstants.BULK_SKU_LEVEL_REBATE_MAX_ROWS_FAILURE_TEMPLATE,
          bulkInternalProcess.getInternalProcessRequestCode(),
          bulkInternalProcess.getCreatedBy(),
          bulkSkuLevelRebateMaxRows);
    }
    long failedCount = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .count();
    if (internalProcessDataFromExcel.size() == failedCount) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
  }

  private void performResultantActionForNewBulkPriceUpdate(
      List<BulkInternalProcessData> bulkInternalProcessDataList, BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> internalProcessDataFromExcel, boolean noRowsDetectedForPriceUpdate) throws Exception {
    if (bulkInternalProcessDataList.isEmpty()) {
      bulkInternalProcess.setNotes(noRowsDetectedForPriceUpdate ?
          BulkProcessValidationErrorMessages.NO_ELIGIBLE_ROWS_DETECTED :
          ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN + bulkPriceUpdateNewMaxRows);
      bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      String templateId, subject;
      if (noRowsDetectedForPriceUpdate) {
        templateId = EmailConstants.BULK_NEW_PRICE_UPDATE_NO_PRICE_CHANGE_TEMPLATE_ID;
        subject = EmailConstants.BULK_NEW_PRICE_UPDATE_NO_PRICE_CHANGE_TEMPLATE;
      } else {
        templateId = EmailConstants.BULK_NEW_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE_ID;
        subject = EmailConstants.BULK_NEW_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE;
      }
      bulkPriceUpdateNewService.sendEmailNotification(templateId, subject, bulkInternalProcess.getInternalProcessRequestCode(),
          bulkInternalProcess.getCreatedBy(), 0, 0, 0, bulkPriceUpdateNewMaxRows);
    }
    long failedCount = bulkInternalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .count();
    if (internalProcessDataFromExcel.size() == failedCount && !noRowsDetectedForPriceUpdate) {
      bulkInternalProcess.setStatus(ProcessStatus.PUBLISHED.name());
    }
  }

  private List<String> getMasterSkuReviewers() {
    return new ArrayList<>(findUsernameByFilter(mskuRoleCode,  Constant.CREATED_DATE,
      VendorProductDataBulkParameters.SORT_DIRECTION_ASC, null));
  }

  private Set<String> findUsernameByFilter(String roleCode, String sortBy, String sortDirection,
      Set<String> roleCodes) {
    int page = 0, totalPage = 0;
    double totalElements = 0;
    Set<String> usernames = new HashSet<>();
    do {
      ListBaseResponse<UserResponse> users =
          this.partnersEngineOutboundService.userFilter(roleCode, sortBy, sortDirection, page,
              roleCodes);
      totalElements = users.getMetadata().getTotalItems();
      totalPage = (int) Math.ceil(totalElements / partnerEngineSize);
      usernames.addAll(users.getContent().stream().map(UserResponse::getUsername).collect(Collectors.toList()));
      page++;
    } while (page != totalPage);
    return usernames;
  }

  public Map<String, List<String>> getReviewers() {
    HashMap<String, List<String>> response = new HashMap<>();
    response.put(VendorProductDataBulkParameters.REVIEWERS, new ArrayList<>(this.findUsernameByFilter(
        VendorProductDataBulkParameters.VENDOR_REVIEWER_ROLE_CODE, Constant.CREATED_DATE,
        VendorProductDataBulkParameters.SORT_DIRECTION_ASC, null)));
    return response;
  }

  public Map<String,List<String>> getIprReviewers() {
    HashMap<String, List<String>> response = new HashMap<>();
    response.put(BulkIPRProductsParameter.IPR_REVIEWERS, new ArrayList<>(
        this.findUsernameByFilter(null, Constants.CREATED_DATE,
            VendorProductDataBulkParameters.SORT_DIRECTION_ASC, new HashSet<>(Arrays.asList(
                iprRoleCodeReviewer.split(Constants.COMMA))))));
    return response;
  }

  private void addToTracker(List<BulkInternalProcessData> bulkInternalProcessData, String updatedBy){
    long numberOfFailedData = bulkInternalProcessData.stream()
        .filter(data -> ProcessStatus.FAILED.name().equals(data.getStatus())).count();
    while (numberOfFailedData != 0) {
      this.trackerService.sendTracker(TrackerConstants.MASTER_PRODUCT_BULK_UPDATE,
          TrackerConstants.MASTER_PRODUCT_UPDATE, TrackerConstants.HYPHEN, TrackerConstants.FAILED, updatedBy);
      numberOfFailedData = numberOfFailedData - 1;
    }
  }

  private List<BulkInternalProcess> updateInternalProcessStatus(List<BulkInternalProcess> bulkInternalProcessList, String status,
      String username, String storeId) {
    List<BulkInternalProcess> updatedBulkInternalProcess = new ArrayList<>();
    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcessList) {
      int isProductPending = internalProcessService.getCountByStoreIdAndStatusAndInternalProcessRequestId(storeId,
          ProcessStatus.PENDING.name(), bulkInternalProcess.getId());
      if (isProductPending == 0) {
        bulkInternalProcess.setStatus(status);
        bulkInternalProcess.setUpdatedBy(username);
        bulkInternalProcess.setUpdatedDate(new Date());
        updatedBulkInternalProcess.add(bulkInternalProcess);
      }
    }
    return updatedBulkInternalProcess;
  }

  @Override
  public Page<BulkInternalProcessSummaryResponse> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, int page, int size) {
    Page<BulkInternalProcess> bulkInternalProcesses =
        this.internalProcessService.bulkInternalProcessSummary(storeId, bulkInternalProcessSummaryRequest, page, size);
    return new PageImpl<>(ResponseHelper.toBulkInternalProcessSummaryResponseList(bulkInternalProcesses.getContent()), PageRequest.of(page, size), bulkInternalProcesses.getTotalElements());
  }

  @Override
  public void bulkInternalProcessCancelRequest(String storeId, String username, String internalProcessRequestCode) {
    this.internalProcessService.bulkInternalProcessCancelRequest(storeId, username, internalProcessRequestCode);
  }

  @Async
  @Trace(dispatcher = true)
  @Override
  public void processStatusUpdate(String storeId, String requestId, String username, String processType) {
    Integer fileBatchSize = getUpdateBatchSizeByProcessType(storeId, processType);
    Page<BulkInternalProcess> bulkInternalProcesses =
        internalProcessService.getAllBulkInternalProcessByStatus(storeId, ProcessStatus.PUBLISHED.name(), PageRequest.of(0, fileBatchSize), processType);
    if (CollectionUtils.isEmpty(bulkInternalProcesses.getContent())) {
      log.info("No internal process status update required for requestId : {} processType {}", requestId, processType);
      return;
    }
    log.info("Started Bulk internal process status update for requestId : {} processType : {}", requestId, processType);
    List<BulkInternalProcess> updatedBulkInternalProcessList = new ArrayList<>();
    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcesses) {
      try {
        log.info("Checking internal process status and update for internal-process-request-code : {}",
            bulkInternalProcess.getInternalProcessRequestCode());
        if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
          bulkProductSuspensionService.setFinalStatusAndNotificationOnSuspension(bulkInternalProcess, storeId);
          continue;
        } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
          masterDataBulkUpdateService.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, storeId);
          continue;
        } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
          vendorProductBulkAssignService.setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
          vendorProductBulkAssignService.setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
          brandAuthorisationService.setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
          bulkConfigurationUpdateService.setFinalStatusAndNotificationOnConfigUpdate(bulkInternalProcess, storeId);
          continue;
        }
        else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
          fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, storeId);
          continue;
        } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)
            || BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
          restrictedKeywordService.setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)
            || BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
          brandAuthorisationService.setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)
                || BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
          bulkVendorActionService.setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
          bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
          bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReview(storeId,
              bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
          .equals(processType)) {
          bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
            storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
          bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
          bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(storeId,
            bulkInternalProcess);
          continue;
        }
        else if(BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
          bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
          bulkIPRProductService.setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(storeId, bulkInternalProcess);
          continue;
        } else if(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
          bulkSkuLevelRebateService.setFinalStatusAndGenerateErrorFileForBulkSkuLevelRebateUpdate(storeId, bulkInternalProcess);
          continue;
        } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
          bulkPriceUpdateNewService.setFinalStatusAndGenerateErrorFileForBulkNewPriceUpdate(storeId, bulkInternalProcess);
          continue;
        }
        checkStatusAndUpdateBulkInternalProcess(bulkInternalProcess, updatedBulkInternalProcessList, username, storeId,
            processType, requestId);
      } catch (Exception e) {
        log.error("Error while updating status for internal-process-request-code : {}",
            bulkInternalProcess.getInternalProcessRequestCode(), e);
      }
    }
    if (CollectionUtils.isNotEmpty(updatedBulkInternalProcessList)) {
      internalProcessService.saveInternalProcesses(updatedBulkInternalProcessList);
    }
    log.info("Done Bulk internal process status update for requestId : {} processType : {}", requestId, processType);
  }

  private void checkStatusAndUpdateBulkInternalProcess(BulkInternalProcess bulkInternalProcess,
      List<BulkInternalProcess> updatedBulkInternalProcessList, String username, String storeId, String processType,
      String requestId) throws Exception {
    log.info("Checking status for internal process request Id : {}", bulkInternalProcess.getId());
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isEmpty(bulkInternalProcessDataList)) {
      log.info("No bulk internal process data found for internalProcessRequestId : {}", bulkInternalProcess.getId());
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
      bulkInternalProcess.setUpdatedBy(username);
      bulkInternalProcess.setUpdatedDate(new Date());
      updatedBulkInternalProcessList.add(bulkInternalProcess);
      return;
    }
    List<String> bulkInternalProcessDataStatusList =
        bulkInternalProcessDataList.stream().map(BulkInternalProcessData::getStatus).distinct()
            .collect(Collectors.toList());
    if (bulkInternalProcessDataStatusList.contains(ProcessStatus.PENDING.name())
        || bulkInternalProcessDataStatusList.contains(ProcessStatus.IN_PROGRESS.name())) {
      log.info("No status update internal process in-progress for internalProcessRequestId : {}",
          bulkInternalProcess.getId());
      return;
    }
    if (bulkInternalProcessDataStatusList.contains(ProcessStatus.COMPLETED.name())) {
      if (!bulkInternalProcessDataStatusList.contains(ProcessStatus.FAILED.name())) {
        bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
      } else {
        bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
      }
    } else {
      bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
    }
    updateSuccessCountAndSendEmailErrorFile(bulkInternalProcess, bulkInternalProcessDataList, username, processType,
        requestId);
    bulkInternalProcessDataList.forEach(bulkInternalProcessData -> bulkInternalProcessData.setMarkForDelete(true));
    internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    updatedBulkInternalProcessList.add(bulkInternalProcess);
  }

  private void updateSuccessCountAndSendEmailErrorFile(BulkInternalProcess bulkInternalProcess,
      List<BulkInternalProcessData> bulkInternalProcessDataList, String username, String processType, String requestId)
      throws Exception {
    if (StringUtils.equalsIgnoreCase(bulkInternalProcess.getStatus(), ProcessStatus.FAILED.name())
        || StringUtils.equalsIgnoreCase(bulkInternalProcess.getStatus(), ProcessStatus.PARTIAL_COMPLETED.name())) {
      List<BulkInternalProcessData> failedInternalProcessDataList = bulkInternalProcessDataList.stream().filter(
          bulkInternalProcessData -> StringUtils.equalsIgnoreCase(ProcessStatus.FAILED.name(),
              bulkInternalProcessData.getStatus())).collect(Collectors.toList());
      bulkInternalProcess.setErrorCount(failedInternalProcessDataList.size());
      bulkInternalProcess.setSuccessCount(bulkInternalProcess.getTotalCount() - failedInternalProcessDataList.size());
      BulkDownloadRequest request = bulkDownloadServiceBeanUtil.getInternalProcessFailedDownloadRequest(
          bulkInternalProcess.getInternalProcessRequestCode(), username, requestId, processType);
      String failedErrorFilePath =
          bulkProcessDownloadService.internalProcessFailedDownloadExcelFile(request, processType);
      bulkInternalProcess.setErrorFilePath(failedErrorFilePath);
    } else {
      bulkInternalProcess.setSuccessCount(bulkInternalProcess.getTotalCount());
      bulkInternalProcess.setErrorCount(0);
    }
    bulkInternalProcess.setEndTime(new Date());
    bulkInternalProcess.setUpdatedBy(username);
    bulkInternalProcess.setUpdatedDate(new Date());
    mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, bulkInternalProcess.getStatus());
  }

  private void updateInternalProcessDataStatus(BulkInternalProcessPendingDataDTO bulkInternalProcessPendingDataDTO) {
    List<BulkInternalProcessData> bulkInternalProcessDataList = internalProcessService
        .getAllProductByParentCodeAndProcessTypeAndInternalProcessRequestId(
            bulkInternalProcessPendingDataDTO.getParentCode(), bulkInternalProcessPendingDataDTO.getProcessType(),
            bulkInternalProcessPendingDataDTO.getInternalProcessRequestId());
    bulkInternalProcessDataList
        .forEach(bulkInternalProcessDatas -> bulkInternalProcessDatas.setStatus(ProcessStatus.IN_PROGRESS.name()));
    internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
  }

  private Integer getFailMinuesBeforeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.SUSPENSION_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.CONFIG_UPDATE_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FAIL_BEFORE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FAIL_BEFORE);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
              SystemParameterConfigNames.BULK_APPROVAL_FAIL_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
              SystemParameterConfigNames.BULK_REJECTION_FAIL_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.MASTER_SKU_BULK_REVIEW_FAIL_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
      .equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FAIL_BEFORE_IN_MINUTES);
    }
    return 0;
  }

  private Integer getAbortMinuesBeforeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.STORE_COPY_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.UPDATE_SALES_CATEGORY_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.SUSPENSION_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.CONFIG_UPDATE_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_ABORT_BEFORE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_ABORT_BEFORE);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
              SystemParameterConfigNames.BULK_APPROVAL_ABORT_BEFORE_IN_MINUTES);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
              SystemParameterConfigNames.BULK_REJECTION_ABORT_BEFORE_IN_MINUTES);
    }
    return 0;
  }

  private Integer getDeleteDaysBeforeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_DELETE_DAYS_BEFORE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.SUSPENSION_DELETE_DAYS_BEFORE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_DAYS_BEFORE);
    }
    else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE);
    }
    else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE);
    }
    else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BEFORE);
    }
    return 0;
  }

  private Integer getDeleteBatchSizeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.SUSPENSION_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.CONFIG_UPDATE_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE);
    }
    return 0;
  }

  private Integer getBatchSizeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_BATCH_SIZE);
    }
      else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.FETCH_PENDING_FBB_L5_SIZE);
    }
    else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BRAND_AUTH_ADD_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BRAND_AUTH_DELETE_FILE_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_APPROVAL_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BRAND_UPDATE_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_REJECTION_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.FETCH_PENDING_MASTER_SKU_REVIEW_BATCH_SIZE);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE);
    } else if(BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_PRICE_REBATE_SIZE);
    }
    else if(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_BATCH_SIZE);
    }
    else if(BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name().equals(processType)){
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_BATCH_SIZE);
    }
    return 1;
  }

  private Integer getTotalBatchSizeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.INTERNAL_BLUK_UPLOAD_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.DELETE_BRAND_AUTH_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.FETCH_PENDING_FBB_L4_ROW_SIZE);
    }
    else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_APPROVAL_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_UPDATE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_REJECTION_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.MASTER_SKU_REVIEW_DB_BATCH_SIZE);
    } else if(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_DB_BATCH_SIZE);
    } else if(BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_PRICE_REBATE_TOTAL_BATCH_SIZE);
    }
    else if(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_DB_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_TOTAL_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_TOTAL_BATCH_SIZE);
    }
    return 1;
  }

  private Integer getUpdateBatchSizeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.UPDATE_SALES_CATEGORY_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.SUSPEND.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.SUSPENSION_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.CONFIGURATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.CONFIG_UPDATE_FILE_BATCH_SIZE);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.DELETE_BRAND_AUTH_STATUS_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.FBB_L4_RESULT_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_UPSERT_FINAL_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.RESTRICTED_KEYWORD_DELETE_FINAL_UPDATE_BATCH_SIZE);
    }else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_ADD_FINAL_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_DELETE_FINAL_UPDATE_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_VENDOR_APPROVAL_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_VENDOR_REJECTION_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_MASTER_SKU_ASSIGNEE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BULK_MASTER_SKU_REVIEW_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()
      .equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.AUTO_APPROVED_BULK_ASSIGNEE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.REBATE_UPDATE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FINAL_UPDATE_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FINAL_UPDATE_SIZE);
    }
    return 0;
  }

  private Integer getFetchBatchSizeByProcessType(String storeId, String processType) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.STORE_COPY_PRODUCT_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.UPDATE_SALES_CATEGORY_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BULK_UPLOAD_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.VENDOR_BULK_ASSIGNMENT_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.DELETE_BRAND_AUTH_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.FBB_L5_CREATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.L4_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.VENDOR_AUTO_ASSIGNMENT_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_UPSERT_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.RESTRICTED_KEYWORD_BULK_DELETE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_AUTH_DELETE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_AUTH_BULK_DELETE_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_APPROVAL_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BRAND_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.BRAND_UPDATE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_REJECTION_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.MASTER_SKU_BULK_ASSIGNEE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.MASTER_SKU_REVIEW_ROW_BATCH_SIZE);
    } else if (BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.PRICE_REBATE_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
        SystemParameterConfigNames.BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId,
          SystemParameterConfigNames.IPR_PRODUCTS_ADD_REVIEW_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_SKU_LEVEL_REBATE_FETCH_BATCH_SIZE);
    } else if (BulkInternalProcessType.BULK_PRICE_UPDATE_NEW.name().equals(processType)) {
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.BULK_NEW_PRICE_UPDATE_FETCH_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.INTERNAL_BRAND_UPDATE.name().equals(processType)){
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE);
    }
    else if (BulkInternalProcessType.INTERNAL_BRAND_NAME_UPDATE.name().equals(processType)){
      return getSystemParameterConfigValue(storeId, SystemParameterConfigNames.INTERNAL_BRAND_NAME_UPDATE_DATA_BATCH_SIZE);
    }
    return 1;
  }

  private Integer getSystemParameterConfigValue(String storeId, String systemParameterConfigNames) {
    return Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, systemParameterConfigNames).getValue());
  }

  @Override
  public void uploadInternalBulkUploadToBulkInternalProcess(String storeId, MasterDataBulkUpdateRequest masterDataBulkUpdateRequest){
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromMasterDataBulkUpdateRequest(storeId, masterDataBulkUpdateRequest);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  @Override
  public void uploadVendorBulkAssignmentProcess(String storeId,
      BulkVendorProductAssignRequest bulkVendorProductAssignRequest) {
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromBulkVendorProductAssignRequest(storeId, bulkVendorProductAssignRequest);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    log.info("Vendor bulk assignment request saved in Internal table request : {}", bulkInternalProcess);
  }

  @Override
  public void uploadBulkRestrictedKeywordProcess(String storeId,
      BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel) {
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromBulkRestrictedKeywordUploadRequest(storeId,
            bulkRestrictedKeywordUploadModel);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    log.info(
        "Bulk restricted keyword upload request saved in Internal table request for internalProcessRequestCode : {}  , {}",
        bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess);
  }

  @Override
  public void uploadBulkBrandAuthProcess(String storeId, BulkBrandAuthUploadModel bulkBrandAuthUploadModel) {
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromBulkBrandAuthUploadRequest(storeId, bulkBrandAuthUploadModel);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    log.info(
        "Bulk Brand Authorization upload request saved in Internal table request for internalProcessRequestCode : {}  , {}",
        bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess);
  }

  @Override
  public void processBulkBrandAuthorisationEvent(String storeId, String processType,
      String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId, internalProcessDataRequestId,
            ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      String errorMessage;
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData =
            internalProcessService.saveInternalProcessData(Collections.singletonList(bulkInternalProcessData)).get(0);
        if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
          BrandAuthCreateRequest brandAuthCreateRequest = RequestHelper.toBrandAuthCreateRequest(
              objectMapper.readValue(bulkInternalProcessData.getData(), BrandAuthAddRequestData.class));
          errorMessage = pcbOutboundService.createBulkBrandAuthorisation(storeId, brandAuthCreateRequest);
        } else {
          BrandAuthDeleteRequest brandAuthDeleteRequest = RequestHelper.toBrandAuthDeleteRequest(
              objectMapper.readValue(bulkInternalProcessData.getData(), BrandAuthDeleteRequestData.class));
          errorMessage = pcbOutboundService.deleteBulkBrandAuthorisation(storeId,
              Collections.singletonList(brandAuthDeleteRequest));
        }
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotEmpty(errorMessage) ? ProcessStatus.FAILED.name() : ProcessStatus.COMPLETED.name(),
            StringUtils.isNotEmpty(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (Exception e) {
        log.error("Exception while processing brand authorization for internalProcessDataRequestId : {} ",
            internalProcessDataRequestId, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  public void uploadBulkReviewProcess(String storeId, BulkReviewUploadModel bulkReviewUploadModel) {
    BulkInternalProcess bulkInternalProcess =
      RequestHelper.getBulkInternalProcessFromBulkReviewUploadRequest(storeId,
        bulkReviewUploadModel);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
    log.info(
      "Bulk Product Review upload request saved in Internal table request for internalProcessRequestCode : {}  , {}",
      bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess);
  }

  @Override
  public void processBulkVendorActionsEvent(String storeId, String processType,
    String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
      internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId,
        internalProcessDataRequestId, ProcessStatus.IN_PROGRESS.name());
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse =
      new GdnRestSingleResponse<>();
    if (Objects.nonNull(bulkInternalProcessData)) {
      String errorMessage = StringUtils.EMPTY;
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData = internalProcessService.saveInternalProcessData(
          Collections.singletonList(bulkInternalProcessData)).get(0);
        BulkApprovalRejectionRequestData bulkVendorActionsModel =
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkApprovalRejectionRequestData.class);
        if (BulkInternalProcessType.BULK_REJECTION.name().equals(processType)) {
          errorMessage =
            productDistributionTaskRepository.vendorRejection(bulkInternalProcessData.getStoreId(),
              bulkInternalProcessData.getSellerCode(), vendorCode,
              RequestHelper.toVendorRejectRequest(bulkVendorActionsModel));
        } else {
          vendorQuickApprovalResponse = productDistributionTaskRepository.vendorQuickApproval(
            bulkInternalProcessData.getStoreId(), bulkInternalProcessData.getSellerCode(),
            RequestHelper.toVendorQuickApprovalRequest(bulkVendorActionsModel, vendorCode));
          errorMessage = generateVendorApprovalErrorMessage(vendorQuickApprovalResponse);
        }
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          StringUtils.isNotEmpty(errorMessage) ?
            ProcessStatus.FAILED.name() :
            ProcessStatus.COMPLETED.name(),
          StringUtils.isNotEmpty(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (ValidationException exception) {
        log.error(
            "Exception while processing bulk vendor action for internalProcessDataRequestId : {} ",
            internalProcessDataRequestId, exception);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), errorMessage);
      }
      catch (IllegalArgumentException exception) {
        log.error(
          "Exception while processing bulk vendor action for internalProcessDataRequestId : {} ",
          internalProcessDataRequestId, exception);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.INVALID_REASON_MESSAGE);
      } catch (Exception ex) {
        log.error(
          "Exception while processing bulk vendor action for internalProcessDataRequestId : {} ",
          internalProcessDataRequestId, ex);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  @Override
  public void processBulkMasterSkuReviewDataEvent(String storeId, String internalProcessDataRequestId)
      throws Exception {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId, internalProcessDataRequestId,
            ProcessStatus.IN_PROGRESS.name());
    if (Objects.isNull(bulkInternalProcessData)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.getMessage());
    }
    bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
    bulkInternalProcessData = internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    try {
      BulkMasterSkuReviewRequestData requestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkMasterSkuReviewRequestData.class);
      ClusterActionResponse clusterActionResponse = masterSkuItemsRepository.performClusterAction(
        BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER.equals(requestData.getAction()) ?
          requestData.getFirstMasterSku() :
          requestData.getSecondMasterSku(), getClusterReviewFeedbackRequest(requestData),
        bulkInternalProcessData.getSellerCode());
      if (CollectionUtils.isNotEmpty(clusterActionResponse.getClusterItemErrorListListResponse())) {
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(),
            clusterActionResponse.getClusterItemErrorListListResponse().stream().findFirst()
                .orElse(new ClusterItemErrorListResponse()).getMessage());
      } else {
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.COMPLETED.name(), StringUtils.EMPTY);
      }
    } catch (ApplicationException e) {
      log.error("Exception while processing bulk master sku review for internalProcessDataRequestId : {} ",
          internalProcessDataRequestId, e);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
          e.getErrorMessage());
    } catch (Exception e) {
      log.error("Exception while processing bulk master sku review for internalProcessDataRequestId : {} ",
          internalProcessDataRequestId, e);
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, ProcessStatus.FAILED.name(),
          Constant.SYSTEM_ERROR);
    }
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
  }

  private ClusterReviewFeedbackRequest getClusterReviewFeedbackRequest(
      BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData) {
    ClusterReviewFeedbackRequest clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
    clusterReviewFeedbackRequest.setAction(
        MasterSkuActions.fromValue(bulKMasterSkuReviewRequestData.getAction()).name());
    if (!BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER.equals(bulKMasterSkuReviewRequestData.getAction())) {
      clusterReviewFeedbackRequest.getItemSkuList().add(bulKMasterSkuReviewRequestData.getFirstMasterSku());
    }
    return clusterReviewFeedbackRequest;
  }

  private String generateVendorApprovalErrorMessage(
    GdnRestSingleResponse<VendorQuickApprovalResponse> vendorQuickApprovalResponse) {
    String errorMessage = StringUtils.EMPTY;
    if (!vendorQuickApprovalResponse.isSuccess()) {
      return vendorQuickApprovalResponse.getErrorMessage();
    } else {
      for (String errorCode : vendorQuickApprovalResponse.getValue().getErrorCodes()) {
        if (ApiErrorCode.BRAND_IS_INREVIEW.getCode().equals(errorCode)) {
          errorMessage =
            errorMessage.concat(ApiErrorCode.BRAND_IS_INREVIEW.getDesc()).concat(Constant.DOT);
        }
        if (ApiErrorCode.SELLER_NOT_AUTHORISED_TO_CREATE_PRODUCT.getCode().equals(errorCode)) {
          errorMessage =
            errorMessage.concat(ApiErrorCode.SELLER_NOT_AUTHORISED_TO_CREATE_PRODUCT.getDesc())
              .concat(Constant.DOT);
        }
        if (ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getCode().equals(errorCode)) {
          errorMessage = errorMessage.concat(ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_NOT_ASSIGNED.getCode().equals(errorCode)) {
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_NOT_ASSIGNED.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_IMAGES_NOT_FOUND.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_IMAGES_NOT_FOUND.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_ITEMS_ARE_MFD_TRUE.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_ITEMS_ARE_MFD_TRUE.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_ITEMS_CANNOT_BE_EMPTY.getCode().equals(errorCode)){
          errorMessage = errorMessage.concat(ApiErrorCode.PRODUCT_ITEMS_CANNOT_BE_EMPTY.getDesc())
            .concat(Constant.DOT);
        }
        if (ApiErrorCode.BRAND_CODE_NOT_ALLOWED.getCode().equals(errorCode)) {
          errorMessage =
            errorMessage.concat(ApiErrorCode.BRAND_CODE_NOT_ALLOWED.getDesc()).concat(Constant.DOT);
        }
        if (ApiErrorCode.PRODUCT_IS_ALREADY_APPROVED.getCode().equals(errorCode)){
          errorMessage=
            errorMessage.concat(ApiErrorCode.PRODUCT_IS_ALREADY_APPROVED.getDesc()).concat(
              Constants.DOT);
        }
      }
      return errorMessage;
    }
  }

  public void processBulkMasterSkuAssigneeEvent(String storeId,
    String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
      internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId,
        internalProcessDataRequestId, ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        List<String> users = getMasterSkuReviewers();
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData =
          internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
        BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(),
            BulkAssigneeMasterSkuReviewRequestData.class);
        String assignee = bulkAssigneeMasterSkuReviewRequestData.getAssignee();
        if (StringUtils.isNotEmpty(assignee) && !users.contains(assignee)) {
          RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), ASSIGNEE_ERROR_MESSAGE);
          internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
          return;
        }
        String errorMessage = masterSkuReviewOutboundService.processBulkUploadAssigneeAction(
          bulkInternalProcessData.getInternalProcessRequestId(),
          RequestHelper.toChangeAssigneeRequest(bulkAssigneeMasterSkuReviewRequestData,
            bulkInternalProcessData.getSellerCode()));
        if (ErrorCategory.DATA_NOT_FOUND.name().equals(errorMessage)) {
          errorMessage = DATA_NOT_FOUND_ERROR_MESSAGE;
        }
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          StringUtils.isNotBlank(errorMessage) ?
            ProcessStatus.FAILED.name() :
            ProcessStatus.COMPLETED.name(),
          StringUtils.isNotBlank(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (Exception ex) {
        log.error("Exception while processing bulk assignee action for master-sku-review for "
          + "internalProcessDataRequestId : {} ", internalProcessDataRequestId, ex);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
          ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  @Override
  public void processAutoApprovedProductsBulkAssignEvent(String storeId, String internalProcessDataRequestId){
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId,
            internalProcessDataRequestId, ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData =
            internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
        BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
            objectMapper.readValue(bulkInternalProcessData.getData(),
                BulkAssignAutoApprovedProductsRequestData.class);
        String errorMessage =
            productAnalyticsOutboundService.processUpdateAssigneeForAutoApprovedProducts(
                bulkInternalProcessData.getInternalProcessRequestId(),
                RequestHelper.toAutoApprovedAssigneeRequest(
                    bulkAssignAutoApprovedProductsRequestData));
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotBlank(errorMessage) ?
                ProcessStatus.FAILED.name() :
                ProcessStatus.COMPLETED.name(),
            StringUtils.isNotBlank(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (Exception e) {
        log.error("Exception while processing bulk assignee action for auto approved products for "
            + "internalProcessDataRequestId : {} ", internalProcessDataRequestId, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  @Override
  public void preProcessBulkPriceRecommendationFile(BulkUpdateProcessDTO bulkUpdateProcessDTO) throws Exception {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    final String bulkProcessCode = UUID.randomUUID().toString();
    fileStorageServiceBean.createBulkFile(bulkUpdateProcessDTO, bulkProcessCode, bulkUpdateProcessDTO.getFileName());
    bulkInternalProcess.setInternalProcessRequestCode(bulkProcessCode);
    BulkCreationCommonUtil.getBulkInternalProcess(bulkUpdateProcessDTO, bulkInternalProcess);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  @Override
  public void processBulkInternalBulkPriceUpdate(InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel,
      boolean updateCampaignPrice) {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        internalProcessService.bulkInternalProcessDataByIdInAndStatus(internalProcessDataDomainEventModel.getStoreId(),
            internalProcessDataDomainEventModel.getInternalProcessDataRequestIdList(),
            ProcessStatus.IN_PROGRESS.name());
    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      try {
        // Change state to processing
        bulkInternalProcessDataList = saveInternalProcessDataToProcessingState(bulkInternalProcessDataList);
        log.info("internalProcessDataRequestId change to processing : {} ",
            internalProcessDataDomainEventModel.getInternalProcessDataRequestId());

        if (updateCampaignPrice) {
          // Check if officer is eligible to update prices for the skus and save error message
          validateAndSaveOfficerToSkuMapping(bulkInternalProcessDataList);
          if (CollectionUtils.isEmpty(bulkInternalProcessDataList)) {
            return;
          }
        }

        Map<String, BulkInternalProcessData> bulkInternalProcessDataMap = bulkInternalProcessDataList.stream()
            .collect(Collectors.toMap(BulkInternalProcessData::getId, Function.identity()));
        ProfileResponse profileResponse =
            getProfileResponse(internalProcessDataDomainEventModel, bulkInternalProcessDataList);
        boolean isMultiPickupPointSeller = ConverterUtil.checkIfMPPIsAllowed(mppAllowedSellers, profileResponse);

        // Check if sellerType is supported for price update
        ConverterUtil.checkIfSellerIsAllowedForPriceUpdate(profileResponse, bulkPriceUpdateAllowedSellers);

        // Get default privilege map
        Map<String, Boolean> privilegedMap = RequestHelper.getDefaultPrivilegeMap();

        // Get product L3 basic response
        BasicProductResponse basicProductInfo =
            getBasicProductResponse(internalProcessDataDomainEventModel, bulkInternalProcessDataList);

        log.info("internalProcessDataRequestId fetched product basic response : {} ",
            internalProcessDataDomainEventModel.getInternalProcessDataRequestId());

        // Convert data to BulkPriceUpdateRequestData
        List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList =
            getBulkPriceUpdateRequestData(bulkInternalProcessDataList);

        // Group requests by ItemSku, same as the existing external update flow
        Map<String, List<BulkPriceUpdateRequestData>> groupRequestByItemSku = bulkPriceUpdateRequestDataList.stream()
            .collect(Collectors.groupingBy(BulkPriceUpdateRequestData::getItemSku));

        List<PickupPointResponse> pickupPointResponseList =
            getPickupPointResponses(bulkPriceUpdateRequestDataList, profileResponse);

        Set<String> cncPickupPointCodes = pickupPointResponseList.stream().filter(PickupPointResponse::isCncActivated)
            .map(PickupPointResponse::getCode).collect(Collectors.toSet());

        for (Map.Entry<String, List<BulkPriceUpdateRequestData>> updateRequest : groupRequestByItemSku.entrySet()) {
          List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO = new ArrayList<>();
          Set<String> sellingPriceUpdatedSkus = new HashSet<>();
          if (!updateCampaignPrice || isSkuEligibleForSellingPriceUpdate(updateRequest.getValue())) {
            // Get existing L5 response
            log.info("internalProcessDataRequestId fetching existing L5 response : {} for items : {} ",
                internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey());

            Map<String, ItemPickupPointListingL3Response> l5ResponseMap =
                getExistingL5Response(updateRequest.getValue(), updateRequest.getKey());
            List<Map<String, String>> productDataFromExcel = new ArrayList<>();
            log.info("internalProcessDataRequestId fetched existing L5 response : {}  for items : {} ",
                internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey());

            // Convert BulkPriceUpdateRequestData to List<Map<String, String>> to re-use existing external update methods
            for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : updateRequest.getValue()) {
              // Populate empty data using existing response
              productDataFromExcel.add(
                  RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, basicProductInfo,
                      l5ResponseMap.get(
                          bulkPriceUpdateRequestData.getPickupPointCode() + bulkPriceUpdateRequestData.getItemSku()),
                      profileResponse, cncPickupPointCodes));
            }
            log.info(
                "internalProcessDataRequestId data converted to to user excel input based using existing response : {} for items : {} ",
                internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey());

            List<Map<String, String>> validationPassedData = new ArrayList<>();

            // Validation after populating missing values
            listBulkUpdateErrorDTO =
                validateDataAfterPopulatingMissingValues(productDataFromExcel,
                    validationPassedData, new ArrayList<>(), pickupPointResponseList);

            log.info("internalProcessDataRequestId validation done : {} for items : {} ",
                internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey());

            // Process L5 update
            List<BulkUpdateSuccessDTO> bulkUpdateSuccessDTOS = new ArrayList<>();
            if (CollectionUtils.isNotEmpty(validationPassedData)) {
              bulkUpdateSuccessDTOS =
                  productLevel3BulkUpdateServiceBean.processBulkUpdateL5(profileResponse.getBusinessPartnerCode(),
                      privilegedMap, listBulkUpdateErrorDTO, validationPassedData, new BulkUpdateErrorCounter(),
                      isMultiPickupPointSeller, bulkInternalProcessDataList.get(0).getCreatedBy(),
                      new ArrayList<>(l5ResponseMap.values()), Constant.INTERNAL_BULK_UPDATE, profileResponse);
              bulkUpdateSuccessDTOS.forEach(bulkUpdateSuccessDTO -> sellingPriceUpdatedSkus.add(
                  convertToOfflineItemId(bulkUpdateSuccessDTO.getProductSku(), bulkUpdateSuccessDTO.getPickupPointCode())));
            }

            log.info("internalProcessDataRequestId update done : {} for items : {} response success : {} failure : {}",
                internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey(),
                bulkUpdateSuccessDTOS, listBulkUpdateErrorDTO);
          }

          if (updateCampaignPrice) {
            updateCampaignPriceAndPopulateErrorMessage(updateRequest, listBulkUpdateErrorDTO, sellingPriceUpdatedSkus);
          }

          List<BulkInternalProcessData> finalStatusBulkInternalProcessData =
              getFinalBulkInternalProcessData(listBulkUpdateErrorDTO, bulkPriceUpdateRequestDataList,
                  bulkInternalProcessDataMap);
          internalProcessService.saveInternalProcessData(finalStatusBulkInternalProcessData);
          log.info("internalProcessDataRequestId process completed done : {} for items : {} ",
              internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), updateRequest.getKey());
        }
      } catch (ApplicationRuntimeException e) {
        log.error("ApplicationException while processing processBulkInternalBulkPriceUpdate for request : {} ",
            internalProcessDataDomainEventModel, e);
        for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
          RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
              ProcessStatus.FAILED.name(), e.getMessage());
        }
        internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
      } catch (Exception e) {
        log.error("Exception while processing processBulkInternalBulkPriceUpdate for request : {} ",
            internalProcessDataDomainEventModel, e);
        for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
          RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
              ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
        }
        internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
      }
    } else {
      log.info("Bulk internal price update process already processed payload : {} ",
          internalProcessDataDomainEventModel);
    }
    log.info("internalProcessDataRequestId finished : {} ",
        internalProcessDataDomainEventModel.getInternalProcessDataRequestId());
  }

  private void updateCampaignPriceAndPopulateErrorMessage(Map.Entry<String, List<BulkPriceUpdateRequestData>> updateRequest,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, Set<String> sellingPriceUpdatedSkus) {
    Map<String, BulkUpdateErrorDTO> bulkUpdateErrorDTOMap = new HashMap<>();
    listBulkUpdateErrorDTO.forEach(bulkUpdateErrorDTO -> {
      bulkUpdateErrorDTO.setReason(BulkProcessValidationErrorMessages.SELLING_PRICE_UPDATE_FAILURE + bulkUpdateErrorDTO.getReason());
      bulkUpdateErrorDTOMap.put(convertToOfflineItemId(bulkUpdateErrorDTO.getProductSku(),
          bulkUpdateErrorDTO.getPickupPointCode()), bulkUpdateErrorDTO);
    });
    Map<String, List<BulkPriceUpdateRequestData>> groupRequestByCampaignCode = updateRequest.getValue()
        .stream()
        .filter(request -> StringUtils.isNotEmpty(request.getCampaignCode())
            && StringUtils.isNotEmpty(request.getCampaignPrice()))
        .collect(Collectors.groupingBy(BulkPriceUpdateRequestData::getCampaignCode));
    for (Map.Entry<String, List<BulkPriceUpdateRequestData>> campaignRequest : groupRequestByCampaignCode.entrySet()) {
      Set<String> campaignPriceUpdateEligibleSkus = new HashSet<>();
      CampaignUpdateResponse campaignUpdateResponse = new CampaignUpdateResponse(new HashMap<>());
      try {
        List<CampaignProductUpdateDto> campaignProductUpdateDtoList = toCampaignProductUpdateDto(campaignRequest,
            campaignPriceUpdateEligibleSkus, updateRequest.getKey(), bulkUpdateErrorDTOMap, listBulkUpdateErrorDTO,
            sellingPriceUpdatedSkus);
        if (CollectionUtils.isNotEmpty(campaignProductUpdateDtoList)) {
          campaignUpdateResponse = campaignRepository.updateCampaignFinalPriceAndQuota(campaignProductUpdateDtoList,
              campaignRequest.getKey());
        }
      } catch (Exception e) {
        log.error("Error occurred while updating campaign price for : {}", campaignRequest);
        for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : campaignRequest.getValue()) {
          String offlineItemId = convertToOfflineItemId(bulkPriceUpdateRequestData.getItemSku(),
              bulkPriceUpdateRequestData.getPickupPointCode());
          populateCampaignUpdateErrorMessage(bulkUpdateErrorDTOMap, listBulkUpdateErrorDTO, sellingPriceUpdatedSkus,
              offlineItemId, Constant.SYSTEM_ERROR);
        }
        continue;
      }
      for (Map.Entry<String, String> itemPPIdStatusMap : campaignUpdateResponse.getItemPPIdStatusMap().entrySet()) {
        populateCampaignUpdateErrorMessage(bulkUpdateErrorDTOMap, listBulkUpdateErrorDTO, sellingPriceUpdatedSkus,
            itemPPIdStatusMap.getKey(), itemPPIdStatusMap.getValue());
      }
      campaignPriceUpdateEligibleSkus.removeAll(campaignUpdateResponse.getItemPPIdStatusMap().keySet());
      BulkUpdateErrorDTO bulkUpdateErrorDTO;
      for (String sku : campaignPriceUpdateEligibleSkus) {
        bulkUpdateErrorDTO = bulkUpdateErrorDTOMap.get(sku);
        if (Objects.nonNull(bulkUpdateErrorDTO)) {
          bulkUpdateErrorDTO.setReason(BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_SUCCESS
              + Constant.NEW_LINE + bulkUpdateErrorDTO.getReason());
        }
      }
    }
  }

  private void populateCampaignUpdateErrorMessage(Map<String, BulkUpdateErrorDTO> bulkUpdateErrorDTOMap,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, Set<String> sellingPriceUpdatedSkus, String offlineItemId,
      String error) {
    BulkUpdateErrorDTO bulkUpdateErrorDTO = bulkUpdateErrorDTOMap.get(offlineItemId);
    if (Objects.nonNull(bulkUpdateErrorDTO)) {
      bulkUpdateErrorDTO.setReason(bulkUpdateErrorDTO.getReason() + Constant.NEW_LINE
          + BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE + error);
    } else {
      StringBuilder errorMessage = new StringBuilder();
      if (sellingPriceUpdatedSkus.contains(offlineItemId)) {
        errorMessage.append(BulkProcessValidationErrorMessages.SELLING_PRICE_UPDATE_SUCCESS + Constant.NEW_LINE);
      }
      errorMessage.append(BulkProcessValidationErrorMessages.CAMPAIGN_PRICE_UPDATE_FAILURE);
      errorMessage.append(error);
      Pair<String, String> itemSkuAndPickupPointCodePair = getItemSkuAndPickupPointCodeFromItemPickupPointId(offlineItemId);
      listBulkUpdateErrorDTO.add(new BulkUpdateErrorDTO(null, itemSkuAndPickupPointCodePair.getFirst(),
          itemSkuAndPickupPointCodePair.getSecond(), errorMessage.toString()));
    }
  }

  private List<CampaignProductUpdateDto> toCampaignProductUpdateDto(Map.Entry<String, List<BulkPriceUpdateRequestData>> campaignRequest,
      Set<String> campaignPriceUpdateEligibleSkus, String itemSku, Map<String, BulkUpdateErrorDTO> bulkUpdateErrorDTOMap,
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, Set<String> sellingPriceUpdatedSkus) throws Exception {
    Map<String, ItemPickupPointListingL3Response> existingL5ResponseMap =
        getExistingL5Response(campaignRequest.getValue(), itemSku);
    Map<String, CampaignProductDetailResponse> campaignProductDetailResponseMap =
        campaignRepository.getCampaignProductDetailsV2(getitemDetailsDtoList(campaignRequest),
                BulkAddCampaignProductQueue.builder().campaignCode(campaignRequest.getKey()).build())
            .stream()
            .collect(Collectors.toMap(campaignProductDetailResponse -> convertToOfflineItemId(
                campaignProductDetailResponse.getItemSku(),
                campaignProductDetailResponse.getPickUpPointCode()), Function.identity()));
    List<CampaignProductUpdateDto> campaignProductUpdateDtoList = new ArrayList<>();
    ItemPickupPointListingL3Response itemPickupPointListingL3Response;
    CampaignProductDetailResponse campaignProductDetailResponse;
    for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : campaignRequest.getValue()) {
      String itemPickupPointId = convertToOfflineItemId(bulkPriceUpdateRequestData.getItemSku(),
          bulkPriceUpdateRequestData.getPickupPointCode());
      itemPickupPointListingL3Response = existingL5ResponseMap.get(
          bulkPriceUpdateRequestData.getPickupPointCode() + bulkPriceUpdateRequestData.getItemSku());
      campaignProductDetailResponse = campaignProductDetailResponseMap.get(itemPickupPointId);
      if (Objects.nonNull(itemPickupPointListingL3Response) && Objects.nonNull(campaignProductDetailResponse)) {
        campaignProductUpdateDtoList.add(CampaignProductUpdateDto.builder()
          .itemSku(bulkPriceUpdateRequestData.getItemSku())
          .itemPickupPointId(itemPickupPointId)
          .finalPrice(Double.parseDouble(bulkPriceUpdateRequestData.getCampaignPrice()))
          .categoryCode(itemPickupPointListingL3Response.getCategoryCode())
          .sellingPrice(itemPickupPointListingL3Response.getPrices().get(0).getSalePrice())
          .originalSellingPrice(itemPickupPointListingL3Response.getPrices().get(0).getSalePrice())
          .quota(campaignProductDetailResponse.getQuota())
          .build());
        campaignPriceUpdateEligibleSkus.add(itemPickupPointId);
      } else {
        populateCampaignUpdateErrorMessage(bulkUpdateErrorDTOMap, listBulkUpdateErrorDTO, sellingPriceUpdatedSkus,
            itemPickupPointId, BulkProcessValidationErrorMessages.RECHECK_CAMPAIGN_CODE_ERROR);
      }
    }
    return campaignProductUpdateDtoList;
  }

  private void validateAndSaveOfficerToSkuMapping(List<BulkInternalProcessData> bulkInternalProcessDataList)
      throws JsonProcessingException {
    List<BulkInternalProcessData> finalStatusBulkInternalProcessData = new ArrayList<>();
    List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList =
        getBulkPriceUpdateRequestData(bulkInternalProcessDataList);
    Map<String, String> offlineItemIdToIdMap = bulkPriceUpdateRequestDataList.stream()
        .collect(Collectors.toMap(bulkPriceUpdateRequestData -> convertToOfflineItemId(bulkPriceUpdateRequestData.getItemSku(),
            bulkPriceUpdateRequestData.getPickupPointCode()), BulkPriceUpdateRequestData::getId));
    Set<String> officerTaggedSkus = priceAnalyticsOutboundService.getOfficerTaggedSkus(offlineItemIdToIdMap.keySet());
    officerTaggedSkus.forEach(offlineItemIdToIdMap::remove);

    Iterator<BulkInternalProcessData> bulkInternalProcessDataIterator = bulkInternalProcessDataList.iterator();
    while (bulkInternalProcessDataIterator.hasNext()) {
      BulkInternalProcessData bulkInternalProcessData = bulkInternalProcessDataIterator.next();
      if (offlineItemIdToIdMap.containsValue(bulkInternalProcessData.getId())) {
        bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
        bulkInternalProcessData.setErrorMessage(BulkProcessValidationErrorMessages.SKU_ID_DOES_NOT_BELONG_TO_EMAIL_ADDRESS);
        finalStatusBulkInternalProcessData.add(bulkInternalProcessData);
        bulkInternalProcessDataIterator.remove();
      }
    }
    internalProcessService.saveInternalProcessData(finalStatusBulkInternalProcessData);
    log.info("Saving BulkInternalProcessData for items : {}", finalStatusBulkInternalProcessData);
  }

  private List<ItemDetailsDto> getitemDetailsDtoList(Map.Entry<String, List<BulkPriceUpdateRequestData>> campaignRequest) {
    List<ItemDetailsDto> itemDetailsDtoList = new ArrayList<>();
    for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : campaignRequest.getValue()) {
      itemDetailsDtoList.add(ItemDetailsDto.builder()
          .itemSku(bulkPriceUpdateRequestData.getItemSku())
          .pickUpPointCode(bulkPriceUpdateRequestData.getPickupPointCode())
          .build());
    }
    return itemDetailsDtoList;
  }

  private boolean isSkuEligibleForSellingPriceUpdate(List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList) {
    boolean sellingPriceUpdateSkuAvailable = false;
    for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : bulkPriceUpdateRequestDataList) {
      if (StringUtils.isNotEmpty(bulkPriceUpdateRequestData.getListPrice()) || StringUtils.isNotEmpty(bulkPriceUpdateRequestData.getSalesPrice())) {
        sellingPriceUpdateSkuAvailable = true;
        break;
      }
    }
    return sellingPriceUpdateSkuAvailable;
  }

  private String convertToOfflineItemId(String itemSku, String pickupPointCode) {
    return itemSku + Constant.HYPHEN + pickupPointCode;
  }

    public static Pair<String, String> getItemSkuAndPickupPointCodeFromItemPickupPointId(String itemPickupPointId) {
    int index = StringUtils.ordinalIndexOf(itemPickupPointId, Constants.HYPHEN, 4);
    String pickupPointCode = itemPickupPointId.substring(index + 1);
    return Pair.of(itemPickupPointId.substring(0, index), pickupPointCode);
  }

  private List<PickupPointResponse> getPickupPointResponses(
      List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList, ProfileResponse profileResponse)
      throws ApplicationException {
    Set<String> uniquePickupPointCodes =
        bulkPriceUpdateRequestDataList.stream().map(BulkPriceUpdateRequestData::getPickupPointCode)
            .collect(Collectors.toSet());
    return this.pickupPointService.getPickupPointSummaryFilter(0,
        PickupPointFilterRequest.builder().businessPartnerCode(profileResponse.getBusinessPartnerCode())
            .codes(uniquePickupPointCodes).build());
  }

  private static List<BulkInternalProcessData> getFinalBulkInternalProcessData(
      List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO, List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList,
      Map<String, BulkInternalProcessData> bulkInternalProcessDataMap) {
    Map<String, BulkUpdateErrorDTO> pickupPointCodeErrorMap = Optional.ofNullable(listBulkUpdateErrorDTO).map(
            list -> list.stream().collect(Collectors.toMap(BulkUpdateErrorDTO::getPickupPointCode, Function.identity())))
        .orElse(Collections.emptyMap());
    List<BulkInternalProcessData> finalStatusBulkInternalProcessData = new ArrayList<>();
    for (BulkPriceUpdateRequestData bulkPriceUpdateRequestData : bulkPriceUpdateRequestDataList) {
      BulkInternalProcessData bulkInternalProcessData =
          getBulkInternalProcessData(bulkInternalProcessDataMap, bulkPriceUpdateRequestData, pickupPointCodeErrorMap);
      finalStatusBulkInternalProcessData.add(bulkInternalProcessData);
    }
    return finalStatusBulkInternalProcessData;
  }

  private static BulkInternalProcessData getBulkInternalProcessData(
      Map<String, BulkInternalProcessData> bulkInternalProcessDataMap,
      BulkPriceUpdateRequestData bulkPriceUpdateRequestData, Map<String, BulkUpdateErrorDTO> pickupPointCodeErrorMap) {
    BulkInternalProcessData bulkInternalProcessData =
        bulkInternalProcessDataMap.get(bulkPriceUpdateRequestData.getId());
    if (pickupPointCodeErrorMap.containsKey(bulkPriceUpdateRequestData.getPickupPointCode())) {
      bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
      bulkInternalProcessData.setErrorMessage(
          pickupPointCodeErrorMap.get(bulkPriceUpdateRequestData.getPickupPointCode()).getReason());
    } else {
      bulkInternalProcessData.setStatus(ProcessStatus.FINISHED.name());
    }
    return bulkInternalProcessData;
  }

  private List<BulkUpdateErrorDTO> validateDataAfterPopulatingMissingValues(
      List<Map<String, String>> productDataFromExcel, List<Map<String, String>> validationPassedData,
      List<Map<String, String>> validationFailedData, List<PickupPointResponse> pickupPointResponseList) {
    List<String> pickupPointCodes =
        pickupPointResponseList.stream().map(PickupPointResponse::getCode).collect(Collectors.toList());
    return bulkUpdateServiceUtil.validateExcelDatasBulkUpdateProduct(productDataFromExcel, pickupPointCodes,
        validationPassedData, validationFailedData, new BulkUpdateErrorCounter(),
        productLevel3BulkUpdateServiceBean.getMinimumPrice(Constant.STORE_ID), null, false, StringUtils.EMPTY);
  }

  private Map<String, ItemPickupPointListingL3Response> getExistingL5Response(
      List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList, String itemSku) throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = getItemPickupPointListingL3Request(
        bulkPriceUpdateRequestDataList, itemSku);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        productLevel3BulkUpdateServiceBean.getItemPickupPointListingL3ResponseList(internalItemPickupPointListFetchSize,
            itemPickupPointListingL3Request);
    return itemPickupPointListingL3ResponseList.stream().collect(
        Collectors.toMap(response -> response.getPickupPointCode() + response.getItemSku(), Function.identity()));
  }

  private static ItemPickupPointListingL3Request getItemPickupPointListingL3Request(
      List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList, String itemSku) {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setProductSku(bulkPriceUpdateRequestDataList.get(0).getProductSku());
    itemPickupPointListingL3Request.setItemSku(itemSku);
    itemPickupPointListingL3Request.setBusinessPartnerCode(bulkPriceUpdateRequestDataList.get(0).getSellerCode());
    itemPickupPointListingL3Request.setPickupPointCodes(
        bulkPriceUpdateRequestDataList.stream().map(BulkPriceUpdateRequestData::getPickupPointCode)
            .collect(Collectors.toSet()));
    return itemPickupPointListingL3Request;
  }

  private List<BulkPriceUpdateRequestData> getBulkPriceUpdateRequestData(
      List<BulkInternalProcessData> bulkInternalProcessDataList) throws JsonProcessingException {
    List<BulkPriceUpdateRequestData> bulkPriceUpdateRequestDataList = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
      BulkPriceUpdateRequestData bulkPriceUpdateRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class);
      bulkPriceUpdateRequestData.setId(bulkInternalProcessData.getId());
      bulkPriceUpdateRequestDataList.add(bulkPriceUpdateRequestData);
    }
    return bulkPriceUpdateRequestDataList;
  }

  private List<BulkInternalProcessData> saveInternalProcessDataToProcessingState(
      List<BulkInternalProcessData> bulkInternalProcessDataList) {
    bulkInternalProcessDataList.forEach(
        bulkInternalProcessData1 -> bulkInternalProcessData1.setStatus(ProcessStatus.PROCESSING.name()));
    bulkInternalProcessDataList = internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
    return bulkInternalProcessDataList;
  }

  private BasicProductResponse getBasicProductResponse(
      InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel,
      List<BulkInternalProcessData> bulkInternalProcessDataList) {
    BasicProductResponse basicProductInfo =
        xProductOutboundService.getBasicProductInfo(internalProcessDataDomainEventModel.getStoreId(),
            bulkInternalProcessDataList.get(0).getParentCode());
    if (Objects.isNull(basicProductInfo) || basicProductInfo.isMarkForDelete() || basicProductInfo.isArchived()
        || basicProductInfo.isTakenDown()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "ProductSku " + bulkInternalProcessDataList.get(0).getParentCode() + " invalid");
    }
    return basicProductInfo;
  }

  private ProfileResponse getProfileResponse(InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel,
      List<BulkInternalProcessData> bulkInternalProcessDataList) throws Exception {
    return businessPartnerRepository.filterByBusinessPartnerCodeV2(internalProcessDataDomainEventModel.getStoreId(),
        bulkInternalProcessDataList.get(0).getSellerCode());
  }

  public void processBulkProductTypeTaggingUpdate(
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel)
    throws JsonProcessingException {
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      internalProcessService.bulkInternalProcessDataByIdInAndStatus(internalBulkUploadDataDomainEventModel.getStoreId(),
        internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestIdList(),
        ProcessStatus.IN_PROGRESS.name());
    if (CollectionUtils.isNotEmpty(bulkInternalProcessDataList)) {
      try {
        // Change state to processing
        List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
          new ArrayList<>();
        log.info("internalProcessDataRequestId change to processing : {} ",
          internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestId());
        formBulkInternalProcessDataList(bulkInternalProcessDataList, bulkPriceProductTypeTaggingRequests);
        fetchUpdatedInternalBulkProcessDataListWithErrorAndStatus(internalBulkUploadDataDomainEventModel, bulkPriceProductTypeTaggingRequests,
          bulkInternalProcessDataList);
        internalProcessService.saveInternalProcessData(bulkInternalProcessDataList);
      } catch (Exception e) {
        log.error("Encountered Exception while updating product type tagging : for requestCode {} "
          + ", error ", bulkInternalProcessDataList.stream()
          .map(BulkInternalProcessData::getInternalProcessRequestCode).collect(Collectors.toSet()), e);
        bulkInternalProcessDataList.forEach(bulkInternalProcessData -> {
          RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
          internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
        });
      }
    }
  }

  private void formBulkInternalProcessDataList(List<BulkInternalProcessData> bulkInternalProcessDataList,
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests)
    throws JsonProcessingException {
    for(BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList){
      BulkPriceProductTypeTaggingRequest bulkPriceProductTypeTaggingRequest =
        objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceProductTypeTaggingRequest.class);
      bulkPriceProductTypeTaggingRequest.setId(bulkInternalProcessData.getId());
      bulkPriceProductTypeTaggingRequests.add(bulkPriceProductTypeTaggingRequest);
    }
  }

  private void fetchUpdatedInternalBulkProcessDataListWithErrorAndStatus(
    InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel,
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests,
    List<BulkInternalProcessData> bulkInternalProcessDataList) {
    List<String> bulkPriceProductTypeTaggingRequestIds =
      bulkPriceProductTypeTaggingRequests.stream().map(BulkPriceProductTypeTaggingRequest::getId)
      .collect(Collectors.toList());
    Map<String, String> rowXErrorMap =
      internalProcessService.updateRemoveBulkProductTypeTagging(bulkPriceProductTypeTaggingRequests, internalBulkUploadDataDomainEventModel.getUpdatedBy());
    bulkInternalProcessDataList.stream().filter(bulkInternalProcessData -> bulkPriceProductTypeTaggingRequestIds.contains(
        bulkInternalProcessData.getId())).forEach(bulkInternalProcessData -> {
      String id = bulkInternalProcessData.getId();
      String errorMessage = rowXErrorMap.get(id);
      String status = StringUtils.isBlank(errorMessage) ? ProcessStatus.COMPLETED.name() :
        ProcessStatus.FAILED.name();
      RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData, status, errorMessage);
    });
  }

  @Override
  public void processBulkRebateUpload(String storeId, String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId, internalProcessDataRequestId,
            ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData = internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
        BulkPriceRebateRequestData bulkPriceRebateRequestData =
            objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceRebateRequestData.class);
        String errorMessage = priceAnalyticsOutboundService.updatePriceRebate(
            RequestHelper.toBulkRebateUpdateRequest(bulkPriceRebateRequestData),
            bulkInternalProcessData.getCreatedBy());
        bulkInternalProcessData.setErrorMessage(errorMessage);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotBlank(errorMessage) ? ProcessStatus.FAILED.name() : ProcessStatus.COMPLETED.name(),
            StringUtils.isNotBlank(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (Exception exception) {
        log.error("Exception while processing bulk assignee action for auto approved products for "
            + "internalProcessDataRequestId : {} ", internalProcessDataRequestId, exception);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
    }
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
  }

  @Override
  public void processBulkSkuLevelRebateUpload(InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel) {
    BulkInternalProcessData bulkInternalProcessData = internalProcessService.bulkInternalProcessDataByIdAndStatus(
        internalProcessDataDomainEventModel.getStoreId(),
        internalProcessDataDomainEventModel.getInternalProcessDataRequestId(), ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
      bulkInternalProcessData = internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
      try {
        BulkSkuLevelRebateRequestData bulkSkuLevelRebateRequestData =
            objectMapper.readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class);
        String errorMessage = priceAnalyticsOutboundService.updateSkuRebate(RequestHelper.toSkuRebateUpdateRequest(
            bulkSkuLevelRebateRequestData));
        bulkInternalProcessData.setErrorMessage(errorMessage);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotBlank(errorMessage) ? ProcessStatus.FAILED.name() : ProcessStatus.COMPLETED.name(),
            StringUtils.isNotBlank(errorMessage) ? errorMessage : StringUtils.EMPTY);
      } catch (Exception exception) {
        log.error("Exception while communicating with price analytics for event model: {} with error message: {}",
            internalProcessDataDomainEventModel, exception.getMessage(), exception);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      log.info("Saving bulkInternalProcessData: {} into database for event model: {}",
          bulkInternalProcessData, internalProcessDataDomainEventModel);
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    } else {
      log.info("Bulk internal process data id was null for event model: {}", internalProcessDataDomainEventModel);
    }
  }

  @Override
  public void processInternalBrandUpdateEvent(
      InternalBrandUpdateEventModel internalBrandUpdateEventModel) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(
            internalBrandUpdateEventModel.getStoreId(),
            internalBrandUpdateEventModel.getInternalProcessDataRequestId(),
            ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        // Change state to processing
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData = internalProcessService.saveInternalProcessData(
            Collections.singletonList(bulkInternalProcessData)).getFirst();

        GdnBaseRestResponse response =
            pbpOutboundService.updateProductBrandName(internalBrandUpdateEventModel,
                bulkInternalProcessData);
        bulkInternalProcessData.setErrorMessage(response.getErrorMessage());
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotBlank(response.getErrorMessage()) ?
                ProcessStatus.FAILED.name() :
                ProcessStatus.COMPLETED.name(), StringUtils.isNotBlank(response.getErrorMessage()) ?
                response.getErrorMessage() :
                StringUtils.EMPTY);
      } catch (Exception e) {
        log.error("Exception while updating the brand for internalBrandUpdateEventModel : {} ",
            internalBrandUpdateEventModel, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
      internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
    }
    log.info("internalProcessDataRequestId finished : {} ",
        internalBrandUpdateEventModel.getInternalProcessDataRequestId());
  }

  @Override
  public void processIPRProductsBulkAddReviewEvent(String storeId, String username,
      String internalProcessDataRequestId) {
    BulkInternalProcessData bulkInternalProcessData =
        internalProcessService.bulkInternalProcessDataByIdAndStatus(storeId, internalProcessDataRequestId,
            ProcessStatus.IN_PROGRESS.name());
    if (Objects.nonNull(bulkInternalProcessData)) {
      try {
        bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name());
        bulkInternalProcessData = internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
        BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
            objectMapper.readValue(bulkInternalProcessData.getData(), BulkAddReviewIPRProductsRequestData.class);
        GdnBaseRestResponse response = productDistributionTaskRepository.performIprAction(storeId, bulkInternalProcessData.getCreatedBy(),
            RequestHelper.toIprActionRequest(bulkAddReviewIPRProductsRequestData,
                bulkInternalProcessData.getCreatedBy()));
        bulkInternalProcessData.setErrorMessage(response.getErrorMessage());
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            StringUtils.isNotBlank(response.getErrorMessage()) ? ProcessStatus.FAILED.name() : ProcessStatus.COMPLETED.name(),
            StringUtils.isNotBlank(response.getErrorMessage()) ? response.getErrorMessage() : StringUtils.EMPTY);
      } catch (ApplicationRuntimeException e) {
        log.error(
            "Exception while processing processing bulk ipr products add or review for "
                + "internalProcessDataRequestId : {} ",
            internalProcessDataRequestId, e);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), e.getErrorMessage());
      } catch (Exception exception) {
        log.error("Exception while processing bulk ipr products add or review for "
            + "internalProcessDataRequestId : {} ", internalProcessDataRequestId, exception);
        RequestHelper.updateInternalProcessDataStatusAndErrorMessage(bulkInternalProcessData,
            ProcessStatus.FAILED.name(), Constant.SYSTEM_ERROR);
      }
    }
    internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData);
  }
}
