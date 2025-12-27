package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessDeleteOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpsertOfflineItemRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.BulkProcessService;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkPendingRequestsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.UnifiedBulkDownloadWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
public class BulkProcessServiceImpl implements BulkProcessService {

  private static final String[] COMPRESSED_FILE_MIMES = {"application/zip", "application/x-rar-compressed"};

  private static final String DOT = ".";
  private static final String COMMA = ", ";
  private static final String XLSX_EXTENSION = ".xlsx";
  private static final String XLS_EXTENSION = ".xls";
  private static final String XLSM_EXTENSION = ".xlsm";
  private static final String ZIP = "zip_";
  private static final String EXCEL_FILE_NAME = "excelFilename";
  private static final String FILE_NAMES = "fileNames";
  private static final String ACTIVE = "ACTIVE";
  private static final String IS_ONLY_EXTERNAL_USER = "isOnlyExternalUser";
  private static final String ACCESSIBLE_PICKUP_POINT_CODES = "accessiblePickupPointCodes";
  private static final Set<String> EXCEL_EXTENSIONS = Set.of(XLSX_EXTENSION, XLS_EXTENSION);

  @Autowired
  private XBulkFeign xBulkFeign;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Value("${multipickuppoint.workflow.enabled:false}")
  private boolean multiPickupPointEnabled;

  @Value("${bulk.process.lisitng.enabled}")
  private boolean bulkProcessListingEnabled;

  @Value("${bundling.allowed.seller.type}")
  private String bundlingAllowedSellerType;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${bulk.creation.avoid.redundant.download}")
  private boolean avoidRedundantDownloadInBulkCreation;

  @Value("${external.creation.uploaded.files}")
  private String fileMapKeys;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private UserPicService userPicService;


  @Override
  public void uploadBulkUpdate(String businessPartnerCode, String username, boolean isOnlyExternalUser,
      MultipartFile request) throws IOException {
    log.info("invoking upload bulk update for business partner code : {} , username : {}", businessPartnerCode,
        username);
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    checkPendingBulkUploads(businessPartnerCode, Constants.BULK_PROCESS_TYPE);
    ProfileResponse businessPartner = this.businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(businessPartner) && ACTIVE.equals(businessPartner.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    Map<String, Boolean> privilegeMap = RequestHelper.getEditAccessibilities();
    privilegeMap.put(IS_ONLY_EXTERNAL_USER, isOnlyExternalUser);
    privilegeMap.put(Accessibilty.IS_PRIVILEGED_TO_EDIT_CNC_STATUS, businessPartner.getCompany().isCncActivated());
    Set<String> accessiblePickupPointCodes = userPicService.fetchAccessiblePickupPointCodes(businessPartner);
    GdnBaseRestResponse response = xBulkFeign.uploadForBulkUpdate(RequestHelper
        .toBulkProcessV2Request(businessPartnerCode, username, privilegeMap, request,
          businessPartner, accessiblePickupPointCodes,
            Constants.BULK_PROCESS_TYPE));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void uploadBulkUpdateEAN(String businessPartnerCode, String username, MultipartFile request)
      throws IOException {
    log.info("invoking upload bulk update ean for business partner code : {} , username : {}",
        businessPartnerCode,
        username);
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.STORE_PRODUCT_MANAGE_PRODUCT_BULK_UPDATE)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    checkPendingBulkUploads(businessPartnerCode, Constants.BULK_PROCESS_TYPE_EAN);
    ProfileResponse businessPartner = this.businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions.checkArgument(
        Objects.nonNull(businessPartner) && ACTIVE.equals(businessPartner.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    BulkProcessV2Request bulkProcessV2Request = RequestHelper.toBulkProcessV2Request(businessPartnerCode, username,
        new HashMap<>(), request, businessPartner, null, Constants.BULK_PROCESS_TYPE_EAN);
    bulkProcessV2Request.setPrivilegedMap(new HashMap<>());
    GdnBaseRestResponse response = xBulkFeign.uploadForBulkUpdateEAN(bulkProcessV2Request);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void uploadBulkUpdateMasterInfo(String businessPartnerCode, String username, BulkBasicInfoWebRequest request)
      throws IOException {
    log.info(
        "invoking upload bulk update of master data for business partner code : {} , username : {}, request : {}",
        businessPartnerCode, username, request);
    checkPendingBulkUploads(businessPartnerCode, Constants.BULK_PROCESS_MASTER_INFO_UPDATE);
    ProfileResponse businessPartner = this.businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions.checkArgument(
        Objects.nonNull(businessPartner) && ACTIVE.equals(businessPartner.getMerchantStatus()),
        ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    log.info("productVideoActivated for bulkProcessCode : {} and flag value {} ", request.getBulkProcessCode(),
        mandatoryParameterHelper.getIsProductVideoActivated());
    GdnBaseRestResponse response = xBulkFeign.uploadBulkBasicInfoFile(mandatoryParameterHelper.getStoreId(),
        mandatoryParameterHelper.getChannelId(), mandatoryParameterHelper.getClientId(),
        mandatoryParameterHelper.getRequestId(), mandatoryParameterHelper.getUsername(),
        RequestHelper.toBulkBasicInfoRequest(businessPartnerCode, username, request, businessPartner,
            Constants.BULK_PROCESS_MASTER_INFO_UPDATE, mandatoryParameterHelper.getIsProductVideoActivated()));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public String upload(String businessPartnerCode, String username, List<String> filenames)
      throws Exception {
    log.info("invoking upload for business partner code : {} , username : {} and filenames : {}", businessPartnerCode,
        username, filenames);
    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    GdnPreconditions
        .checkArgument(Objects.nonNull(profileResponse) && ACTIVE.equals(profileResponse.getMerchantStatus()),
            ErrorMessages.INVALID_BUSINESS_PARTNER_CODE + businessPartnerCode);
    String excelFilename = null;
    String zipFilename = null;
    List<MultipartFile> multipartFileList =
      fileStorageService.downloadMultiPartFile(username, filenames);
    for (MultipartFile file : multipartFileList) {
      GdnPreconditions.checkArgument(!file.isEmpty(), ErrorMessages.EMPTY_FILE);
    }

    boolean excelFileExist = false;
    boolean zipFileExist = false;
    MultipartFile excelFile = null;
    List<MultipartFile> zipFileList = new ArrayList<>();
    for (MultipartFile file : multipartFileList) {
      if (file.getName().substring(file.getName().lastIndexOf(DOT)).equals(XLSM_EXTENSION) ||
          file.getName().substring(file.getName().lastIndexOf(DOT)).equals(XLSX_EXTENSION)) {
        excelFilename = file.getName();
        excelFile = file;
        excelFileExist = true;
        break;
      }
    }
    for (MultipartFile file : multipartFileList) {
      if (isCompressedFileInMime(file)) {
        zipFilename = file.getName();
        zipFileList.add(file);
      }
    }
    if (!excelFileExist){
      throw new ValidationException(ErrorMessages.EXCEL_ZIP_FILE_TYPE_INVALID);
    }
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(excelFile.getInputStream())), "UTF-8");
    String excelFileType = null;
    if (excelFilename.endsWith(XLSX_EXTENSION)) {
      excelFileType = FileType.XLSX.name().toLowerCase();
      if (multiPickupPointEnabled) {
        XSSFWorkbook workbook = new XSSFWorkbook(excelFile.getInputStream());
        XSSFSheet sheet = workbook.getSheet(Constants.BULK_UPSERT_SHEET);
        if (Objects.nonNull(sheet)) {
          uploadForBulkUpsertOfflineItems(username, businessPartnerCode, profileResponse,
            excelFile);
          return excelFilename;
        }
      }
    } else if (excelFilename.endsWith(XLSM_EXTENSION)) {
      excelFileType = FileType.XLSM.name().toLowerCase();
    }
    Map<String, String> files = new HashMap<>();
    files.put(excelFileType, excelData);
    int index = 0;
    for (MultipartFile zipFile : zipFileList) {
      String zipData = new String(Base64.encodeBase64(IOUtils.toByteArray(zipFile.getInputStream())), "UTF-8");
      files.put(ZIP + (++index), zipData);
    }
    Set<String> accessiblePickupPointCodes =
        userPicService.fetchAccessiblePickupPointCodes(profileResponse);

    Map<String, String> args = new HashMap<String, String>();
    args.put(EXCEL_FILE_NAME, excelFilename);
    args.put(ACCESSIBLE_PICKUP_POINT_CODES,
        accessiblePickupPointCodes.stream().collect(Collectors.joining(COMMA)));
    args.put(IS_ONLY_EXTERNAL_USER, mandatoryParameterHelper.isExternalOnly());

    checkPendingBulkUploads(businessPartnerCode, Constants.BULK_PRODUCT_CREATION_TYPE);
    GdnBaseRestResponse response =
        xBulkFeign.upload(RequestHelper.toBulkProcessUploadRequest(businessPartnerCode, args, files));
    ResponseHelper.validateResponse(response);
    return excelFilename + COMMA + zipFilename;
  }

  @Override
  public String uploadV2(String businessPartnerCode, String username, List<String> filenames,
    String processType) throws Exception {
    log.info("invoking upload for business partner code : {} , username : {} and filenames : {} "
      + "processType : {} ", businessPartnerCode, username, filenames, processType);
    ProfileResponse profileResponse =
        businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    RequestHelper.validateActiveBusinessPartner(businessPartnerCode, profileResponse);
    String excelFilename = StringUtils.EMPTY;
    List<String> allowedFilenames =
        filenames.stream().filter(StringUtils::isNotBlank).filter(name -> {
          String lower = name.toLowerCase();
          return EXCEL_EXTENSIONS.stream().anyMatch(lower::endsWith);
        }).toList();
    List<MultipartFile> multipartFileList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(allowedFilenames) && StringUtils.equals(processType,
      Constants.BULK_UPSERT)) {
      multipartFileList = fileStorageService.downloadMultiPartFile(username, allowedFilenames);
    }
    MultipartFile excelFile = null;
    for (MultipartFile file : multipartFileList) {
      GdnPreconditions.checkArgument(!file.isEmpty(), ErrorMessages.EMPTY_FILE);
      excelFilename = file.getName();
      excelFile = file;
      break;
    }
    if (StringUtils.isNotBlank(excelFilename)) {
        uploadForBulkUpsertOfflineItems(username, businessPartnerCode, profileResponse, excelFile);
        return excelFilename;
    }
    Set<String> accessiblePickupPointCodes =
        userPicService.fetchAccessiblePickupPointCodes(profileResponse);
    Map<String, String> args = new HashMap<>();
    args.put(EXCEL_FILE_NAME, excelFilename);
    args.put(FILE_NAMES, String.join(Constants.UNICODE_DELIMITER, filenames));
    args.put(ACCESSIBLE_PICKUP_POINT_CODES,
        accessiblePickupPointCodes.stream().collect(Collectors.joining(COMMA)));
    args.put(IS_ONLY_EXTERNAL_USER, mandatoryParameterHelper.isExternalOnly());
    String bulkProcessType = Constants.BULK_PRODUCT_CREATION_TYPE;
    if (StringUtils.equals(Constants.CONVERTED_PRODUCT_CREATION_UPLOAD, processType)) {
      bulkProcessType = Constants.CONVERTED_PRODUCT_CREATION_UPLOAD;
      args.put(Constants.CONVERTED_PRODUCT_CREATION_UPLOAD, "true");
    }
    checkPendingBulkUploads(businessPartnerCode, bulkProcessType);
    GdnBaseRestResponse response = xBulkFeign.upload(
        RequestHelper.toBulkProcessUploadRequest(businessPartnerCode, args, new HashMap<>()));
    ResponseHelper.validateResponse(response);
    return excelFilename;
  }

  public boolean isCompressedFileInMime(MultipartFile file) throws Exception {
    return isInCompressedFileMimes(URLConnection.guessContentTypeFromName(file.getOriginalFilename()));
  }

  @Override
  public void uploadBulkUpdateForBulkArchive(String username, String businessPartnerCode, MultipartFile multipartFile)
      throws Exception {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties.contains(Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    GdnBaseRestResponse response = xBulkFeign.bulkArchiveItemSkus(
        RequestHelper.toBulkProcessUpdateRequest(businessPartnerCode, username, new HashMap<>(), multipartFile, null,
            Constants.BULK_PROCESS_TYPE));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void uploadBulkUpdateForInStoreUpdate(String username, String businessPartnerCode, MultipartFile multipartFile)
      throws Exception {
    Map<String, Boolean> privilegeMap = RequestHelper.getEditAccessibilities();
    ProfileResponse profileResponse = this.businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    if (!RequestHelper.checkPrivilegeEditO2O(profileResponse, privilegeMap)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    this.checkPendingBulkUploads(businessPartnerCode, Constants.OFF2ON_BULK_PROCESS_TYPE);
    GdnBaseRestResponse response = xBulkFeign.bulkUpdateOff2On(RequestHelper
        .toBulkProcessUpdateRequest(businessPartnerCode, username, new HashMap<>(), multipartFile, profileResponse,
            Constants.OFF2ON_BULK_PROCESS_TYPE));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<PromoUpdateProductResponse> fetchPromoUpdatedProductNotes(String bulkProcessCode) {
    GdnRestListResponse<BulkProcessNotesResponse> bulkProcessNotesResponseList =
        this.xBulkFeign.filterPromoBulkProcessNotesByBulkProcessCode(bulkProcessCode);
    ResponseHelper.validateResponse(bulkProcessNotesResponseList);
    List<PromoUpdateProductResponse> responses =
        Optional.ofNullable(bulkProcessNotesResponseList.getContent()).orElse(new ArrayList<>()).stream()
            .filter(BulkProcessNotesResponse::isPromoNote).map(ResponseHelper::toPromoUpdateProductResponse)
            .collect(Collectors.toList());
    return responses;
  }

  @Override
  public WholesaleCountWebResponse fetchWholeSaleConfigCount(String bulkProcessCode) {
    GdnRestSingleResponse<WholeSaleCountResponse> response =
        this.xBulkFeign.filterWholeSaleConfigBulkProcessNotesByBulkProcessCode(bulkProcessCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toWholeCountWebResponse(response.getValue());
  }

  @Override
  public BulkPendingRequestsWebResponse checkPendingBulkProcess(String type, String businessPartnerCode,
      String bulkProcessType) {
    GdnRestSingleResponse<BulkPendingRequestsResponse> bulkPendingRequestsResponse =
        xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(type, businessPartnerCode, bulkProcessType);
    ResponseHelper.validateResponse(bulkPendingRequestsResponse);
    BulkPendingRequestsWebResponse bulkPendingRequestsWebResponse = BulkPendingRequestsWebResponse.builder()
        .bulkUpdateStatusFlag(bulkPendingRequestsResponse.getValue().isBulkUpdateStatusFlag())
        .pendingRequestsCount(bulkPendingRequestsResponse.getValue().getPendingRequestsCount()).build();
    return bulkPendingRequestsWebResponse;
  }

  private boolean isInCompressedFileMimes(String mime) {
    for (String archiveMime : COMPRESSED_FILE_MIMES) {
      if (archiveMime.equals(mime)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public UnifiedBulkDownloadWebResponse downloadProductUnifiedTemplate(String businessPartnerCode) throws Exception {
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response;
    if (userPicService.shouldRestrictAccess(null)) {
      Set<String> pickupPoints = mandatoryParameterHelper.getPickupPoints();
      response = xBulkFeign.downloadProductUnifiedTemplateV2(businessPartnerCode,
        new PickupPointCodesRequestDTO(pickupPoints));
    } else {
      response = xBulkFeign.downloadProductUnifiedTemplate(businessPartnerCode);
    }
    ResponseHelper.validateBulkDownloadFileContentResponse(response, businessPartnerCode);
    UnifiedBulkDownloadWebResponse unifiedBulkDownloadWebResponse =
        UnifiedBulkDownloadWebResponse.builder().filePath(response.getValue().getFilePath())
            .build();
    return unifiedBulkDownloadWebResponse;
  }

  @Override
  public SystemParameterConfigResponse getBulkSystemParameterConfig(String variableName) {
    GdnRestSingleResponse<SystemParameterConfigResponse> responseGdnRestSingleResponse =
        xBulkFeign.findOne(variableName);
    ResponseHelper.validateResponse(responseGdnRestSingleResponse);
    return responseGdnRestSingleResponse.getValue();
  }

  @Override
  public void uploadBulkUpdateForBulkArchiveProductSkus(String username, String businessPartnerCode,
      MultipartFile multipartFile) throws Exception {
    List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
    if (!accessibility.contains(Accessibilty.FUNCTION_BULK_ARCHIVE_PRODUCTS)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    this.checkPendingBulkUploads(businessPartnerCode, Constants.ARCHIVE_BULK_PROCESS_TYPE);
    GdnBaseRestResponse response = xBulkFeign.bulkArchiveProductSkus(RequestHelper
        .toBulkProcessUpdateRequest(businessPartnerCode, username, new HashMap<>(), multipartFile, null,
            Constants.BULK_PROCESS_TYPE));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void uploadBulkForWorkOrderCreation(String username, String type, String businessPartnerCode,
      MultipartFile multipartFile) throws Exception {

    if (!Constants.VALID_BULK_WORK_ORDER_TYPE.contains(type)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
          String.format(ErrorMessages.INVALID_WORK_ORDER_TYPE, type));
    }

    ProfileResponse profileResponse = businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);

    if (!productBundlingEnabled || !Arrays.asList(bundlingAllowedSellerType.split(Constants.COMMA_DELIMITER_NO_SPACE))
        .contains(profileResponse.getCompany().getMerchantType())) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
          ErrorMessages.INVALID_SELLER_TYPE_FOR_ASSEMBLY);
    }

    this.checkPendingBulkUploads(businessPartnerCode, type);

    GdnBaseRestResponse response = xBulkFeign.createWorkOrder(
        RequestHelper.toBulkProcessUpdateRequest(businessPartnerCode, username, new HashMap<>(), multipartFile, null, type));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void uploadBulkSubjectToVatSkus(String businessPartnerCode, String fileName, String bulkProcessCode) {
    BulkProcessSubjectToVatRequest bulkProcessSubjectToVatRequest = new BulkProcessSubjectToVatRequest();
    bulkProcessSubjectToVatRequest.setBusinessPartnerCode(businessPartnerCode);
    bulkProcessSubjectToVatRequest.setFileName(fileName);
    bulkProcessSubjectToVatRequest.setBulkProcessCode(bulkProcessCode);
    this.checkPendingBulkUploads(businessPartnerCode, Constants.SUBJECT_TO_VAT_TYPE);
    xBulkFeign.bulkUploadSubjectToVat(bulkProcessSubjectToVatRequest);
  }

  @Override
  public void uploadBulkDeleteOfflineItems(String requestId, String businessPartnerCode,
    String username, String clientId, MultipartFile file) throws Exception {
    BulkProcessDeleteOfflineItemRequest bulkProcessDeleteOfflineItemRequest = RequestHelper
      .toBulkProcessDeleteOfflineItemRequest(businessPartnerCode, username, clientId, file,
        userPicService.fetchAccessiblePickupPointCodes(null));
    GdnBaseRestResponse response =
      xBulkFeign.bulkUploadDeleteOfflineItems(bulkProcessDeleteOfflineItemRequest);
    ResponseHelper.validateResponse(response);
  }

  private void checkPendingBulkUploads(String businessPartnerCode, String bulkProcessType) {
    GdnRestSingleResponse<BulkPendingRequestsResponse> pendingRequestsResponse =
        xBulkFeign.checkPendingBulkRequestsByBusinessPartnerCode(Constants.BULK_UPLOAD_TYPE, businessPartnerCode,
            bulkProcessType);
    ResponseHelper.validateResponse(pendingRequestsResponse);
    if (pendingRequestsResponse.getValue().getPendingRequestsCount() > 0) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.PENDING_UPLOAD_FILES);
    }
  }

  @Override
  public void uploadForBulkUpsertOfflineItems(String username, String businessPartnerCode,
    ProfileResponse profileResponse, MultipartFile multipartFile) throws Exception {
    Set<String> accessiblePickupPoints =
      userPicService.fetchAccessiblePickupPointCodes(profileResponse);

    checkPendingBulkUploads(businessPartnerCode, BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue());
    BulkProcessUpsertOfflineItemRequest bulkProcessUpsertOfflineItemRequest =
      RequestHelper.toBulkProcessUpsertOfflineItemRequest(businessPartnerCode, username,
        multipartFile, accessiblePickupPoints);
    GdnBaseRestResponse response =
      xBulkFeign.uploadForBulkUpsertOfflineItems(bulkProcessUpsertOfflineItemRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<BulkProcessStatusListingWebResponse> fetchBulkProcessListingWebResponse(
    String storeId, String requestId, String businessPartnerCode, String bulkProcessType,
    Optional<List<String>> bulkProcessCodes, boolean estimationsNeeded, Integer page, Integer size) throws Exception {
    List<String> processCodes = new ArrayList<>();
    GdnRestListResponse<BulkProcessStatusListingResponse> listingResponse =
      new GdnRestListResponse<>();
   if(bulkProcessCodes.isPresent()){
     processCodes = bulkProcessCodes.get();
   }
   if(bulkProcessListingEnabled) {
     listingResponse =
       xBulkFeign.fetchBulkProcessStatusListing(storeId, Constants.CHANNEL_ID, Constants.CLIENT_ID,
         requestId, bulkProcessType, businessPartnerCode, processCodes, estimationsNeeded, page, size);
   }
    log.info("Listing response fetched from Bulk for merchant : {} was : {} ", businessPartnerCode,
      listingResponse);
    if(CollectionUtils.isEmpty(listingResponse.getContent())){
      return new PageImpl<>(Collections.EMPTY_LIST, PageRequest.of(page, size),
        0);
    }
    ResponseHelper.validateResponse(listingResponse);
    List<BulkProcessStatusListingWebResponse> listingWebResponses =
      RequestHelper.toBulkProcessStatusListingWebResponse(listingResponse);
    return new PageImpl<>(listingWebResponses, PageRequest.of(page, size),
      listingResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public BulkProcessResponse getBulkProcessResponseByProcessCode( String bulkProcessCode) {
    GdnRestSingleResponse<BulkProcessResponse> bulkProcessResponse = xBulkFeign.getBulkProcessByProcessCode(
        mandatoryParameterHelper.getStoreId(), mandatoryParameterHelper.getChannelId(),
        mandatoryParameterHelper.getClientId(), mandatoryParameterHelper.getRequestId(),
        mandatoryParameterHelper.getUsername(), bulkProcessCode);
    ResponseHelper.validateResponse(bulkProcessResponse);
    return bulkProcessResponse.getValue();
  }

  @Override
  public void uploadExcelFile(String businessPartnerCode, String username, List<String> fileNames,
    String processType) throws Exception {
    if (avoidRedundantDownloadInBulkCreation) {
      uploadV2(businessPartnerCode, username, fileNames, processType);
    } else {
      upload(businessPartnerCode, username, fileNames);
    }
  }

  @Override
  public void uploadExternalFiles(String businessPartnerCode, String username, String zipFileName,
      Map<String, String> files, String pickupPointCode, String bulkProcessCode) throws Exception {
    Set<String> expectedFileNames = Set.of(fileMapKeys.split(Constants.COMMA_DELIMITER_NO_SPACE));
    GdnPreconditions.checkArgument(!zipFileName.isEmpty(), ErrorMessages.EMPTY_FILE);
    GdnPreconditions.checkArgument(!files.isEmpty() && expectedFileNames.equals(files.keySet()),
        ErrorMessages.EMPTY_FILE);
    GdnPreconditions.checkArgument(files.values().stream().filter(StringUtils::isNotEmpty)
            .allMatch(name -> name.toLowerCase().endsWith(XLSX_EXTENSION)),
        ErrorMessages.EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE);
    log.info("invoking upload for business partner code : {} , username : {} zipName : {} and "
            + "filenames : {} " + "bulkProcessCode : {} ", businessPartnerCode, username,
        zipFileName,
        files, bulkProcessCode);
    ProfileResponse profileResponse =
        businessPartnerService.filterByBusinessPartnerCode(businessPartnerCode);
    RequestHelper.validateActiveBusinessPartner(businessPartnerCode, profileResponse);
    if (StringUtils.isNotEmpty(pickupPointCode)) {
      Set<String> accessiblePickupPointCodes =
          userPicService.fetchAccessiblePickupPointCodes(profileResponse);
      if (CollectionUtils.isNotEmpty(accessiblePickupPointCodes)
        && !accessiblePickupPointCodes.contains(pickupPointCode)) {
        throw new ValidationException(
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS_EN);
      }
    }
    checkPendingBulkUploads(businessPartnerCode, Constants.BULK_PRODUCT_EXTERNAL_CREATION);
    GdnBaseRestResponse response = xBulkFeign.externalUpload(
        RequestHelper.toBulkProcessExternalUploadRequest(zipFileName, files, bulkProcessCode,
            pickupPointCode, mandatoryParameterHelper.isExternalOnly(), businessPartnerCode));
    ResponseHelper.validateResponse(response);
  }
}
