package com.gdn.mta.product.service;

import java.io.File;
import java.io.FileOutputStream;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.mta.product.service.util.PbpStockAlertConverterUtil;
import com.gdn.partners.pbp.commons.constants.BulkUpdateExcelHeaders;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.newrelic.api.agent.Trace;

@Service
@Transactional(readOnly = true)
public class ProductStockAlertServiceBean implements ProductStockAlertService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductStockAlertServiceBean.class);
  private static final String SYSTEM = "System";
  private static final String CM_MERCHANT = "CM";
  private static final String NON_CM_MERCHANT = "NON-CM MERCHANT";
  private static String LANGUAGE_IN = "-in";
  private static String LANGUAGE_EN = "-en";

  private static String MIN_STOCK = "minimum_stock_update";
  private static String OOS_UPDATE = "oos_stock_update";
  private static String INVENTORY_FULFILLMENT_BLIBLI = "BL";
  private static final NumberFormat formatter = new DecimalFormat("0");
  private static final String FILE_SEPARATOR = "/";
  private static final String FILE_NAME_SEPARATOR = "_";
  private static final String EXCEL_EXTENSION = ".xlsx";
  private static final String EXCEL_TRUE_VALUE = "1";
  private static final String EXCEL_FALSE_VALUE = "0";
  private static final String EXCEL_NOT_APPLICABLE_VALUE = "-";

  @Autowired
  private ProductStockAlertRepository productStockAlertRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductNotificationService productNotificationService;
  
  @Autowired
  private PbpStockAlertConverterUtil pbpStockAlertConverterUtil;
  
  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private ProductMailEventPublisher productMailEventPublisher;

  @Trace(dispatcher=true)
  @Override
  @Async("autoBatchUpdateItemViewConfigExecutor")
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void autoBatchUpdateItemViewConfig() {
    try {
      long startTime = System.currentTimeMillis();
      LOGGER.info("Starting auto batch update item view config");
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
      int batchSize = Integer.parseInt(this.applicationProperties.getBatchUpdateSize());
      int maxStockAlertAttempt = Integer.parseInt(this.applicationProperties.getMaxStockAlertAttempt()) + 1;
      
      Page<PbpStockAlert> pbpStockAlertPage = this.productStockAlertRepository.findByOosAlertAttempt(maxStockAlertAttempt, PageRequest.of(0, batchSize));
      Long totalRecords = pbpStockAlertPage.getTotalElements();

      LOGGER.info("invoking updateItemViewConfig : {}", pbpStockAlertPage);
      this.updateItemViewConfig(pbpStockAlertPage);
      while (pbpStockAlertPage.hasNext()) {
        Pageable nextPage = pbpStockAlertPage.nextPageable();
        pbpStockAlertPage = this.productStockAlertRepository.findByOosAlertAttempt(maxStockAlertAttempt, nextPage);
        LOGGER.info("invoking updateItemViewConfig : {}", pbpStockAlertPage);
        this.updateItemViewConfig(pbpStockAlertPage);
      }
      long endTime = System.currentTimeMillis();
      LOGGER.info("Auto batch update item view config success, totalRecords :{}, totalTime: {}",
          totalRecords, (endTime - startTime));
    } catch (Exception e) {
      LOGGER.error(
          "Error when invoking autoBatchUpdateItemViewConfig at ProductStockAlertServiceBean", e);
    }
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  private void updateItemViewConfig(Page<PbpStockAlert> pbpStockAlerts) throws Exception {
    for (PbpStockAlert pbpStockAlert : pbpStockAlerts) {
      ProductLevel3Inventory inventory =
          this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndGdnSku(
              pbpStockAlert.getBusinessPartnerCode(), pbpStockAlert.getGdnSku());
      LOGGER.info("invoking updateItemViewConfig : {}", inventory);
      try {
        if (!inventory.isWebSyncStock() && inventory.getWebAvailable() <= 0) {
          ItemViewConfigRequest request = new ItemViewConfigRequest();
          request.setBuyable(false);
          request.setDiscoverable(false);
          this.productLevel3Service.updateItemViewConfig(request, pbpStockAlert.getGdnSku(), StringUtils.EMPTY);
        } else if (inventory.isWebSyncStock() && inventory.getWarehouseAvailable() <= 0) {
          ItemViewConfigRequest request = new ItemViewConfigRequest();
          request.setBuyable(false);
          request.setDiscoverable(false);
          this.productLevel3Service.updateItemViewConfig(request, pbpStockAlert.getGdnSku(), StringUtils.EMPTY);
        }
      } catch (Exception e) {
        LOGGER.info("Error invoking updateItemViewConfig : {}, {}", inventory, e);
      }
      
    }
  }
  
  @Override
  public List<String> findGdnSkuStockAlertByBusinessPartnerCode(String businesspartnerCode) {
    try {
      return this.productStockAlertRepository.findGdnSkuStockAlertByBusinessPartnerCode(
          businesspartnerCode, Integer.parseInt(this.applicationProperties.getMaxStockAlertAttempt()));
    } catch (Exception e) {
      LOGGER.error("error invoking gdn sku product stock alert by business partner code at service",
          e);
    }
    return null;
  }

  @Override
  public PbpStockAlert findPbpStockAlertByGdnSkuCode(String gdnSku) {
    return this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalse(gdnSku);
  }

  @Override
  public PbpStockAlert findOnePbpStockAlertByGdnSkuCode(String gdnSku) {
    List<PbpStockAlert> results = this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(gdnSku);
    if (!CollectionUtils.isEmpty(results)) {
      return results.get(0);
    }
    return null;
  }

  @Override
  public PbpStockAlert findOnePbpStockAlertByGdnSkuCodeNoMFDCheck(String gdnSku) {
    List<PbpStockAlert> results = this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(gdnSku);
    if (!CollectionUtils.isEmpty(results)) {
      return results.get(0);
    }
    return null;
  }

  private PbpStockAlert findOneDeletedPbpStockAlertByGdnSkuCode(String gdnSku) {
    List<PbpStockAlert> results = this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(gdnSku);
    if (!CollectionUtils.isEmpty(results)) {
      return results.get(0);
    }
    return null;
  }
  
  @Override
  @Transactional(readOnly = false)
  public PbpStockAlert createPbpStockAlert(PbpStockAlert pbpStockAlert) {
    if (StringUtils.isEmpty(pbpStockAlert.getId())) {
      return this.productStockAlertRepository.save(pbpStockAlert);
    } else {
      throw new IllegalArgumentException("cannot create pbp stock alert because id is exists !");
    }
  }

  @Override
  @Transactional(readOnly = false)
  public PbpStockAlert updatePbpStockAlert(PbpStockAlert pbpStockAlert) {
    if (!StringUtils.isEmpty(pbpStockAlert.getId())
        && this.productStockAlertRepository.findById(pbpStockAlert.getId()).orElse(null) != null) {
      return this.productStockAlertRepository.save(pbpStockAlert);
    } else {
      throw new IllegalArgumentException(
          "cannot create pbp stock alert because id is not exists !");
    }
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  private void updateOosAlertAttempt(List<PbpStockAlert> pbpStockAlerts) throws Exception {
    for (PbpStockAlert pbpStockAlert : pbpStockAlerts) {
      if (pbpStockAlert.getIsOos()) {
        this.updateOosAlertAttempt(pbpStockAlert.getGdnSku(), pbpStockAlert.getOosAlertAttempt() + 1);
      } else {
        this.updateOosAlertAttempt(pbpStockAlert.getGdnSku(), 0);
      }
    }
  }
  
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  private void updateOosAlertAttempt(String gdnSku, int oosAlertAttempt) {
    this.productStockAlertRepository.updateOosAlertAttempt(gdnSku, oosAlertAttempt);
  }

  private void sendMailProductStockAlert(List<PbpStockAlert> pbpStockAlerts, ProfileResponse businessPartner) throws Exception {
    List<PbpStockAlert> pbpStockAlertsForOOSProducts = new ArrayList<>();
    List<PbpStockAlert> pbpStockAlertsForMinStockProducts = new ArrayList<>();
    List<String> dateOfExecution = new ArrayList<>();
    for(PbpStockAlert pbpStockAlert : pbpStockAlerts) {
      if(pbpStockAlert.getIsOos()) {
        pbpStockAlertsForOOSProducts.add(pbpStockAlert);
      }
      if(pbpStockAlert.getIsMinimumStock()) {
        pbpStockAlertsForMinStockProducts.add(pbpStockAlert);
      }
    }
    if(CollectionUtils.isNotEmpty(pbpStockAlertsForMinStockProducts)) {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd");
      String date = simpleDateFormat.format(new Date());
      dateOfExecution.add(date);
      String language = (businessPartner.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN);
      List<List<String>> itemSkuData = new ArrayList<>();
      List<String> itemCode = new ArrayList<>();
      List<String> itemName = new ArrayList<>();
      for (PbpStockAlert pbpStockAlert : pbpStockAlertsForMinStockProducts) {
        itemCode.add(pbpStockAlert.getGdnSku());
        itemName.add(pbpStockAlert.getProductName());
      }
      itemSkuData.add(itemCode);
      itemSkuData.add(itemName);
      itemSkuData.add(dateOfExecution);
      generateAndUploadExcel(itemCode, businessPartner, MIN_STOCK);
      ProductMailDomainEvent productMailDomainEvent = ProductMailDomainEvent.builder().productDatas(itemSkuData)
          .merchantCode(businessPartner.getBusinessPartnerCode()).build();
      if (LANGUAGE_EN.equals(language)) {
        this.productMailEventPublisher.publishMinStockItemSkuEventEn(productMailDomainEvent);
      } else if (LANGUAGE_IN.equals(language)) {
        this.productMailEventPublisher.publishMinStockItemSkuEvent(productMailDomainEvent);
      }
    }
      LOGGER.info("invoking send email product stock alert at service. Email for merchant : ",
          businessPartner.getCompany().getBusinessPartnerName());
     if(CollectionUtils.isNotEmpty(pbpStockAlertsForOOSProducts)) {
       SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd");
       String date = simpleDateFormat.format(new Date());
       dateOfExecution.add(date);
       String language = (businessPartner.getCompany().isInternationalFlag() ? LANGUAGE_EN : LANGUAGE_IN);
       List<List<String>> itemSkuData = new ArrayList<>();
       List<String> itemCode = new ArrayList<>();
       List<String> itemName = new ArrayList<>();
       for (PbpStockAlert pbpStockAlert : pbpStockAlertsForOOSProducts) {
         itemCode.add(pbpStockAlert.getGdnSku());
         itemName.add(pbpStockAlert.getProductName());
       }
       itemSkuData.add(itemCode);
       itemSkuData.add(itemName);
       itemSkuData.add(dateOfExecution);
       generateAndUploadExcel(itemCode, businessPartner, OOS_UPDATE);
       ProductMailDomainEvent productMailDomainEvent = ProductMailDomainEvent.builder().productDatas(itemSkuData)
           .merchantCode(businessPartner.getBusinessPartnerCode()).build();
       if (LANGUAGE_EN.equals(language)) {
         this.productMailEventPublisher.publishOOSItemSkuEventEn(productMailDomainEvent);
       } else if (LANGUAGE_IN.equals(language)) {
         this.productMailEventPublisher.publishOOSItemSkuEvent(productMailDomainEvent);
       }
     }
  }

  private void sendNotificationProductStockAlert(String businessPartnerCode) throws Exception {
    Integer pbpStockMinimum = this.productStockAlertRepository.countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(businessPartnerCode);
    Integer pbpStockOos = this.productStockAlertRepository.countGdnSkuWithOosStockAlertByBusinessPartnerCode(businessPartnerCode,
            Integer.parseInt(this.applicationProperties.getMaxStockAlertAttempt()));
    if (pbpStockMinimum > 0) {
      this.productNotificationService.sendProductStockMinimalNotification(businessPartnerCode, pbpStockMinimum);
    }
    
    if (pbpStockOos > 0) {
      this.productNotificationService.sendProductStockOosNotification(businessPartnerCode, pbpStockOos);
    }
  }

  private void generateAndUploadExcel(List<String> itemSkus, ProfileResponse businessPartner, String updateReason) {
    Workbook workbook = generateWorkbook(itemSkus, businessPartner);
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    String date = simpleDateFormat.format(new Date());
    String filename = new StringBuilder().append(businessPartner.getBusinessPartnerCode()).append(FILE_NAME_SEPARATOR)
        .append(updateReason).append(FILE_NAME_SEPARATOR).append(date).append(EXCEL_EXTENSION).toString();
    String directoryPath =
        new StringBuilder().append(this.applicationProperties.getDirectoryStockAlertsExcel()).append(FILE_SEPARATOR)
            .append(updateReason).append(FILE_SEPARATOR).append(businessPartner.getBusinessPartnerCode())
            .append(FILE_SEPARATOR).append(date).toString();
    String filePath = new StringBuilder().append(directoryPath).append(FILE_SEPARATOR).append(filename).toString();
    try {
      createDirectories(directoryPath);
      createXLSXFile(filePath, workbook);
    } catch (Exception e) {
      LOGGER.error("Error uploading excel to NFS : {}", e.getMessage());
    }
  }

  private Workbook generateWorkbook(List<String> itemSkus, ProfileResponse businessPartner) {
    Workbook workbook = null;
    boolean isBlibliFulfillment =
        businessPartner.getCompany().getInventoryFulfillment().equals(INVENTORY_FULFILLMENT_BLIBLI);

    Map<Integer, List<String>> products = new TreeMap<Integer, List<String>>();
    List<String> header = new ArrayList<String>();
    int pickupPointIndex = BulkUpdateExcelHeaders.PICKUP_POINT_DEFAULT_COLUMN_INDEX;
    int index = 0;
    generateHeaders(header, isBlibliFulfillment);

    for (String itemSku : itemSkus) {
      ProductLevel3Summary productLevel3Summary = null;
      try {
        productLevel3Summary =
            this.productLevel3Service.findSummaryByGdnSku(businessPartner.getBusinessPartnerCode(), itemSku);
      } catch (Exception e) {
        LOGGER.error("Product not found for : {}, error : {}, ", itemSku, e.getMessage(), e);
      }
      if (Objects.nonNull(productLevel3Summary)) {

        List<String> response = new ArrayList<String>();
        generateExcelData(response, isBlibliFulfillment, productLevel3Summary);
        products.put(index++, response);
      }
    }

    try {
      workbook = generateXLFile(header, products, businessPartner.getPickupPoints());
    } catch (Exception e) {
      LOGGER.error("Exception while creating workbook : {}", e.getMessage());
    }
    generateValidationWorkbookTemplateBulkUpdateSXSSF(workbook, businessPartner.getPickupPoints(),
        pickupPointIndex);

    return workbook;
  }

  private void generateHeaders(List<String> header, Boolean isBlibliFulfillment) {
    header.add(BulkUpdateExcelHeaders.BLIBLI_PRODUCT_SKU);
    header.add(BulkUpdateExcelHeaders.PARENT_PRODUCT_NAME);
    header.add(BulkUpdateExcelHeaders.BLIBLI_SKU);
    header.add(BulkUpdateExcelHeaders.NAMA_PRODUK);
    header.add(BulkUpdateExcelHeaders.SKU_CODE);
    header.add(BulkUpdateExcelHeaders.MERCHANT_SKU);
    header.add(BulkUpdateExcelHeaders.HARGA);
    header.add(BulkUpdateExcelHeaders.HARGA_PENJULAN);
    header.add(BulkUpdateExcelHeaders.STOCK);
    header.add(BulkUpdateExcelHeaders.TOKO_OR_GUDANG);
    header.add(BulkUpdateExcelHeaders.EXTERNAL_SKU_STATUS);
    if (isBlibliFulfillment) {
      header.add(BulkUpdateExcelHeaders.WAREHOUSE_STOCK);
    }
  }

  private void generateExcelData(List<String> response, Boolean isBlibliFulfillment,
      ProductLevel3Summary productLevel3Summary) {
    response.add(productLevel3Summary.getProductSku());
    response.add(productLevel3Summary.getProductName());
    response.add(productLevel3Summary.getItemSku());
    response.add(productLevel3Summary.getItemName());
    response.add(productLevel3Summary.getSkuCode());
    response.add(productLevel3Summary.getMerchantSku());
    response.add(formatter.format(productLevel3Summary.getPrices().get(0).getPrice()));
    response.add(formatter.format(productLevel3Summary.getPrices().get(0).getSalePrice()));

    response.add((productLevel3Summary.getSynchronizeStock() == Boolean.TRUE) ?
        EXCEL_NOT_APPLICABLE_VALUE :
        String.valueOf(productLevel3Summary.getAvailableStockLevel2()));
    response.add(productLevel3Summary.getPickupPointCode());
    response.add(Boolean.TRUE.equals(productLevel3Summary.getViewConfigs().get(0).getBuyable())
        && Boolean.TRUE.equals(productLevel3Summary.getViewConfigs().get(0).getDisplay()) ?
        BulkUpdateExcelHeaders.SKU_STATUS_ONLINE :
        BulkUpdateExcelHeaders.SKU_STATUS_OFFLINE);
    if (isBlibliFulfillment) {
      response.add(String.valueOf(productLevel3Summary.getAvailableStockLevel1() == null ?
          EXCEL_NOT_APPLICABLE_VALUE :
          productLevel3Summary.getAvailableStockLevel1()));
    }
  }

  private void generateValidationWorkbookTemplateBulkUpdateSXSSF(Workbook workbook,
      List<PickupPointDTO> pickupPoints, int pickupPointIndex) {
    Sheet dataSheet = workbook.getSheet("Data");
    DataValidationHelper dataValidationHelper = null;
    DataValidationConstraint dataValidationConstraint = null;
    DataValidation dataValidation = null;
    CellRangeAddressList cellRangeAddressList = new CellRangeAddressList(1, 500000, pickupPointIndex, pickupPointIndex);
    dataValidationHelper = dataSheet.getDataValidationHelper();
    dataValidationConstraint =
        dataValidationHelper.createFormulaListConstraint("Toko!$A$1:$A$" + (pickupPoints.size() + 1));
    dataValidation = dataValidationHelper.createValidation(dataValidationConstraint, cellRangeAddressList);
    dataSheet.addValidationData(dataValidation);
  }

  public static SXSSFWorkbook generateXLFile(List<String> headerList,
      Map<Integer, List<String>> products, List<PickupPointDTO> pickupPoints) throws Exception {
    SXSSFWorkbook workbook = new SXSSFWorkbook();
    Sheet dataSheet = workbook.createSheet("Data");
    Sheet pickupPointSheet = workbook.createSheet("Toko");
    ((SXSSFSheet) dataSheet).trackAllColumnsForAutoSizing();
    int rowindex = 0;
    Row row;
    for (PickupPointDTO pickupPoint : pickupPoints) {
      row = pickupPointSheet.createRow((short) rowindex++);
      Cell cell = row.createCell((short) 0);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getCode());

      cell = row.createCell((short) 1);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      cell.setCellValue(pickupPoint.getName());
    }

    row = dataSheet.createRow((short) 0);
    int cellIndex = 0;
    row.setHeightInPoints(BulkUpdateExcelHeaders.HEADER_ROW_HEIGHT);
    CellStyle cellStyle = workbook.createCellStyle();
    setLightGreenCellBackground(cellStyle);
    setThinBlackBorderCellStyle(cellStyle);
    for (String header : headerList) {
      Cell cell = row.createCell((short) cellIndex++);
      cell.setCellType(Cell.CELL_TYPE_STRING);
      setCellStyleForExternalProductList(cell, header, cellStyle);
      cell.setCellValue(header);
    }
    Set<Integer> keyid = products.keySet();
    int rowid = 1;
    for (Integer key : keyid) {
      row = dataSheet.createRow(rowid++);
      List<String> attributes = products.get(key);
      int cellid = 0;
      for (String attribute : attributes) {
        Cell cell = row.createCell(cellid++);
        cell.setCellValue(Optional.ofNullable(attribute).orElse(StringUtils.EMPTY));
      }
    }
    if (MapUtils.isNotEmpty(products)) {
      for (int i = 0; i < dataSheet.getRow(dataSheet.getLastRowNum()).getLastCellNum(); i++) {
        dataSheet.autoSizeColumn(i);
      }
    }
    return workbook;
  }

  private static void setThinBlackBorderCellStyle(CellStyle cellStyle) {
    cellStyle.setBorderBottom(BorderStyle.THIN);
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderTop(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setBottomBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setLeftBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setTopBorderColor(IndexedColors.BLACK.getIndex());
    cellStyle.setRightBorderColor(IndexedColors.BLACK.getIndex());
  }

  private static void setLightGreenCellBackground(CellStyle cellStyle) {
    cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    cellStyle.setFillForegroundColor(IndexedColors.LIGHT_GREEN.getIndex());
  }

  private static void setCellStyleForExternalProductList(Cell cell, String headerValue,
      CellStyle cellStyle) {
    if (BulkUpdateExcelHeaders.BLIBLI_PRODUCT_SKU.equals(headerValue)
        || BulkUpdateExcelHeaders.PARENT_PRODUCT_NAME.equals(headerValue)
        || BulkUpdateExcelHeaders.EXTERNAL_SKU_STATUS.equals(headerValue)) {
      cell.setCellStyle(cellStyle);
    }
  }

  public static void createDirectories(String path) {
    File directories = new File(path);
    if (!directories.exists()) {
      directories.mkdirs();
    }
  }

  public static void createXLSXFile(String path, Workbook workbook) throws Exception {
    FileOutputStream fileOutputStream = new FileOutputStream(new File(path));
    workbook.write(fileOutputStream);
  }

  @Trace(dispatcher=true)
  @Override
  @Async("sendMailAndNotificationStockAlertExecutor")
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void sendMailAndNotification() throws Exception {
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
      int maxStockAlertAttempt = Integer.parseInt(this.applicationProperties.getMaxStockAlertAttempt());
      List<String> businessPartnerCodes = this.productStockAlertRepository.findListBusinessPartnerMinimumStock(maxStockAlertAttempt);
      if (CollectionUtils.isNotEmpty(businessPartnerCodes)){
        for (String businessPartnerCode : businessPartnerCodes) {
          try {
            List<PbpStockAlert> pbpStockAlerts = this.productStockAlertRepository
                .findPbpStockAlertByBusinessPartnerCode(businessPartnerCode, maxStockAlertAttempt);
            ProfileResponse businessPartner =
              this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
                pbpStockAlerts.get(0).getBusinessPartnerCode());
            if (Objects.nonNull(businessPartner) && MerchantStatus.ACTIVE
                .equals(MerchantStatus.valueOf(businessPartner.getMerchantStatus()))) {
              if (Objects.nonNull(businessPartner.getCompany()) && CM_MERCHANT
                  .equalsIgnoreCase(businessPartner.getCompany().getMerchantType()) && !businessPartner.getCompany()
                  .isOfflineToOnlineFlag()) {
                if (CollectionUtils.isNotEmpty(pbpStockAlerts)) {
                  this.updateOosAlertAttempt(pbpStockAlerts);
                  this.sendMailProductStockAlert(pbpStockAlerts, businessPartner);
                  this.sendNotificationProductStockAlert(businessPartnerCode);
                }
              }
            }
          }catch (Exception e) {
            LOGGER.error("Error sending email notification products with stock alert, businessPartnerCode : {}",
                businessPartnerCode, e);
          }
        }
      }
    } catch (Exception e) {
      LOGGER.error("Error sending email notification products with stock alert", e);
    }
  }

  @Override
  public List<String> findListBusinessPartnerMinimumStock(int maxStockAlertAttempt) {
    try {
      return this.productStockAlertRepository
          .findListBusinessPartnerMinimumStock(maxStockAlertAttempt);
    } catch (Exception e) {
      LOGGER
          .error("Error fetching merchants by maxStockAlertAttempt : {} ", maxStockAlertAttempt, e);
      return new ArrayList<>();
    }
  }

  @Override
  public List<PbpStockAlert> findPbpStockAlertByBusinessPartnerCode(String businessPartnerCode,
      int maxStockAlertAttempt) throws Exception {
    return this.productStockAlertRepository
        .findPbpStockAlertByBusinessPartnerCode(businessPartnerCode, maxStockAlertAttempt);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateStockAlertForMailer(PbpStockAlert pbpStockAlert) {
    this.productStockAlertRepository
        .updateOosAlertAttemptById(pbpStockAlert.getId(), pbpStockAlert.getOosAlertAttempt() + 1);
  }

  @Override
  public void sendMailAndNotificationForStockAlert(List<PbpStockAlert> pbpStockAlertList,
      ProfileResponse businessPartner) throws Exception {
    this.sendMailProductStockAlert(pbpStockAlertList, businessPartner);
    this.sendNotificationProductStockAlert(businessPartner.getBusinessPartnerCode());
  }

  @Override
  public void updateDeletedPbpStockAlertByGdnSkuCode(String gdnSku) throws Exception {
    try {
      PbpStockAlert pbpStockAlert =
          this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalse(gdnSku);
      pbpStockAlert.setMarkForDelete(true);
      this.productStockAlertRepository.save(pbpStockAlert);
    } catch (Exception e) {
      LOGGER.error(
          "Error updateDeletedPbpStockAlertByGdnSkuCode products with stock alert : {}",
          gdnSku, e);
    }
  }
  
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void updateOOS(Level2InventoryOosEvent messageEvent, String itemName, int retry) throws Exception {
    LOGGER.info("updateOOS with param : {},{},{}", messageEvent, itemName, retry);
    PbpStockAlert pbpStockAlert = this.findOnePbpStockAlertByGdnSkuCodeNoMFDCheck(messageEvent.getLevel2Id());
    Integer minimum_stock =
        this.productBusinessPartnerRepository.getMinimumStockByGdnProductItemSku(messageEvent.getLevel2Id());
    if (pbpStockAlert != null && !pbpStockAlert.isMarkForDelete()) {
      if (pbpStockAlert.getEventTimestamp() == null || messageEvent.getTimestamp() >= pbpStockAlert.getEventTimestamp().getTime()) {
        pbpStockAlert.setAvailableStock(0);
        pbpStockAlert.setMinimumStock(minimum_stock);
        pbpStockAlert.setProductName(itemName);
        pbpStockAlert.setIsMinimumStock(false);
        pbpStockAlert.setIsOos(true);
        pbpStockAlert.setOosDate(new Date(messageEvent.getTimestamp()));
        pbpStockAlert.setOosAlertAttempt(0);
        pbpStockAlert.setEventTimestamp(new Date(messageEvent.getTimestamp()));
        pbpStockAlert.setPickupPointCode(messageEvent.getPickupPointCode());
      } else {
        LOGGER.info("pbpStockAlert productLevel3Aggregator is up to date with message : {}", messageEvent);
        return;
      }
    } else if(Objects.isNull(pbpStockAlert)) {
      pbpStockAlert = this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(messageEvent.getStoreId(),
              messageEvent.getLevel2MerchantCode(), messageEvent.getLevel2Id(), itemName, 0,
              minimum_stock, false, true, new Date(messageEvent.getTimestamp()), messageEvent.getPickupPointCode());
    }
    
    LOGGER.info("updateNonOOS with value : {}", pbpStockAlert, retry);
    if(pbpStockAlert != null && retry < 3){
      pbpStockAlert.setPickupPointCode(messageEvent.getPickupPointCode());
      try {
        this.productStockAlertRepository.save(pbpStockAlert);
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.updateOOS ! Retry again", ofe);
        LOGGER.info("ProductStockAlertServiceBean.updateOOS retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateOOS(messageEvent, itemName, retry+1);
      } catch (Exception e) {
        LOGGER.error("Exception occured in ProductStockAlertServiceBean.updateOOS ! Retry again", e);
        LOGGER.info("ProductStockAlertServiceBean.updateOOS retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateOOS(messageEvent, itemName, retry+1);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void updateNonOOS(Level2InventoryNonOosEvent messageEvent, String itemName, int retry) throws Exception {
    LOGGER.info("updateNonOOS with param : {},{},{}", messageEvent, itemName, retry);
    PbpStockAlert pbpStockAlert = this.findOnePbpStockAlertByGdnSkuCode(messageEvent.getLevel2Id());
    if (pbpStockAlert != null) {
      if (pbpStockAlert.getEventTimestamp() == null ||  messageEvent.getTimestamp() >= pbpStockAlert.getEventTimestamp().getTime()) {
        pbpStockAlert.setProductName(itemName);
        pbpStockAlert.setIsOos(false);
        pbpStockAlert.setOosDate(null);
        pbpStockAlert.setOosAlertAttempt(0);
        pbpStockAlert.setEventTimestamp(new Date(messageEvent.getTimestamp()));
        pbpStockAlert.setPickupPointCode(messageEvent.getPickupPointCode());
      } else {
        LOGGER.info("updateNonOOS productLevel3Aggregator is up to date with message : {}", messageEvent);
        return;
      }
    } else {
      LOGGER.error("updateNonOOS PbpStockAlert Not found with messageEvent : {}, itemName : {}", messageEvent, itemName);
      return;
    }
    
    LOGGER.info("updateNonOOS with value : {}", pbpStockAlert, retry);
    if (pbpStockAlert != null && retry < 3) {
      try {
        this.productStockAlertRepository.save(pbpStockAlert);
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.updateNonOOS ! Retry again", ofe);
        LOGGER.info("ProductStockAlertServiceBean.updateNonOOS retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateNonOOS(messageEvent, itemName, retry+1);
      } catch (Exception e) {
        LOGGER.error("Exception occured in ProductStockAlertServiceBean.updateNonOOS ! Retry again", e);
        LOGGER.info("ProductStockAlertServiceBean.updateNonOOS retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateNonOOS(messageEvent, itemName, retry+1);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void updateMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent,
      String itemName, int retry) throws Exception {
    LOGGER.info("updateMinimumStock with param : {},{},{}", messageEvent, itemName, retry);
    PbpStockAlert pbpStockAlert = this.findOnePbpStockAlertByGdnSkuCodeNoMFDCheck(messageEvent.getGdnSku());
    if (pbpStockAlert != null && !pbpStockAlert.isMarkForDelete()) {
      if (pbpStockAlert.getEventTimestamp() == null ||  messageEvent.getTimestamp() >= pbpStockAlert.getEventTimestamp().getTime()) {
        pbpStockAlert.setAvailableStock(messageEvent.getAvailableStock());
        pbpStockAlert.setMinimumStock(messageEvent.getMinimumStock());
        pbpStockAlert.setProductName(itemName);
        pbpStockAlert.setIsMinimumStock(true);
        pbpStockAlert.setIsOos(messageEvent.getAvailableStock() == 0);
        pbpStockAlert.setPickupPointCode(messageEvent.getPickupPointCode());
        if(messageEvent.getAvailableStock() == 0) {
          pbpStockAlert.setOosDate(new Date(messageEvent.getTimestamp()));
        } else {
          pbpStockAlert.setOosDate(null);
        }
        if (!pbpStockAlert.getIsOos()) {
          pbpStockAlert.setOosAlertAttempt(0);
        }
        pbpStockAlert.setEventTimestamp(new Date(messageEvent.getTimestamp()));
      } else {
        LOGGER.info("updateMinimumStock productLevel3Aggregator is up to date with message : {}", messageEvent);
        return;
      }
    } else if (Objects.isNull(pbpStockAlert)) {
      pbpStockAlert =
        this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(messageEvent.getStoreId(),
          messageEvent.getBusinessPartnerCode(), messageEvent.getGdnSku(), itemName,
          messageEvent.getAvailableStock(), messageEvent.getMinimumStock(), true,
          messageEvent.getAvailableStock() == 0, new Date(messageEvent.getTimestamp()),
          messageEvent.getPickupPointCode());
    }
    
    LOGGER.info("updateMinimumStock with value : {}", pbpStockAlert, retry);
    if(pbpStockAlert != null && retry < 3){
      try {
        this.productStockAlertRepository.save(pbpStockAlert);
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.updateMinimumStock ! Retry again", ofe);
        LOGGER.info("ProductStockAlertServiceBean.updateMinimumStock retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateMinimumStock(messageEvent, itemName, retry+1);
      } catch (Exception e) {
        LOGGER.error("Exception occured in ProductStockAlertServiceBean.updateMinimumStock ! Retry again", e);
        LOGGER.info("ProductStockAlertServiceBean.updateMinimumStock retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateMinimumStock(messageEvent, itemName, retry+1);
      }
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void updateNonMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent,
      String itemName, int retry)  throws Exception {
    LOGGER.info("updateNonMinimumStock with param : {},{},{}", messageEvent, itemName, retry);
    PbpStockAlert pbpStockAlert = this.findOnePbpStockAlertByGdnSkuCodeNoMFDCheck(messageEvent.getGdnSku());
    if (pbpStockAlert != null && !pbpStockAlert.isMarkForDelete()) {
      if (pbpStockAlert.getEventTimestamp() == null ||  messageEvent.getTimestamp() >= pbpStockAlert.getEventTimestamp().getTime()) {
        pbpStockAlert.setAvailableStock(messageEvent.getAvailableStock());
        pbpStockAlert.setMinimumStock(messageEvent.getMinimumStock());
        pbpStockAlert.setProductName(itemName);
        pbpStockAlert.setIsMinimumStock(false);
        pbpStockAlert.setIsOos(messageEvent.getAvailableStock() == 0);
        pbpStockAlert.setPickupPointCode(messageEvent.getPickupPointCode());
        if(messageEvent.getAvailableStock() == 0) {
          pbpStockAlert.setOosDate(new Date(messageEvent.getTimestamp()));
        } else {
          pbpStockAlert.setOosDate(null);
        }
        if (!pbpStockAlert.getIsOos()) {
          pbpStockAlert.setOosAlertAttempt(0);
        }
        pbpStockAlert.setEventTimestamp(new Date(messageEvent.getTimestamp()));
      } else {
        LOGGER.info("updateNonMinimumStock productLevel3Aggregator is up to date with message : {}", messageEvent);
        return;
      }
    } else if (Objects.isNull(pbpStockAlert)){
      pbpStockAlert =
          this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(messageEvent.getStoreId(),
              messageEvent.getBusinessPartnerCode(), messageEvent.getGdnSku(), itemName,
              messageEvent.getAvailableStock(), messageEvent.getMinimumStock(), false,
              messageEvent.getAvailableStock() == 0, new Date(messageEvent.getTimestamp()),
            messageEvent.getPickupPointCode());

    }
    
    LOGGER.info("updateNonMinimumStock with value : {}", pbpStockAlert, retry);
    if(pbpStockAlert != null && retry < 3){
      try {
        this.productStockAlertRepository.save(pbpStockAlert);
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.updateNonMinimumStock ! Retry again", ofe);
        LOGGER.info("ProductStockAlertServiceBean.updateNonMinimumStock retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateNonMinimumStock(messageEvent, itemName, retry+1);
      } catch (Exception e) {
        LOGGER.error("Exception occured in ProductStockAlertServiceBean.updateNonMinimumStock ! Retry again", e);
        LOGGER.info("ProductStockAlertServiceBean.updateNonMinimumStock retry again with param : {},{},{}", messageEvent, itemName, retry+1);
        this.updateNonMinimumStock(messageEvent, itemName, retry+1);
      }
    }
  }

  
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class,
      propagation = Propagation.REQUIRES_NEW)
  public void archiveGdnSku(String itemSku, boolean archived, int retry) throws Exception {
  LOGGER.info("archiveGdnSku with param : {},{},{}", itemSku, archived, retry);
    PbpStockAlert pbpStockAlert = this.findOnePbpStockAlertByGdnSkuCode(itemSku);
    if (pbpStockAlert != null && archived) {
      pbpStockAlert.setMarkForDelete(true);
    } else if (pbpStockAlert == null && !archived) {
      pbpStockAlert = this.findOneDeletedPbpStockAlertByGdnSkuCode(itemSku);
      if (pbpStockAlert != null) {
        pbpStockAlert.setMarkForDelete(false);
      }
    }
    
    LOGGER.info("archiveGdnSku with value : {}", pbpStockAlert, retry);
    if(pbpStockAlert != null && retry < 3){
      try {
        this.productStockAlertRepository.save(pbpStockAlert);
      } catch (ObjectOptimisticLockingFailureException ofe) {
        LOGGER.error("ObjectOptimisticLockingFailureException occured in ProductStockAlertServiceBean.archiveGdnSku ! Retry again", ofe);
        LOGGER.info("ProductStockAlertServiceBean.archiveGdnSku retry again with param : {},{},{}", itemSku, archived, retry+1);
        this.archiveGdnSku(itemSku, archived, retry+1);
      } catch (Exception e) {
        LOGGER.error("Exception occured in ProductStockAlertServiceBean.archiveGdnSku ! Retry again", e);
        LOGGER.info("ProductStockAlertServiceBean.archiveGdnSku retry again with param : {},{},{}", itemSku, archived, retry+1);
        this.archiveGdnSku(itemSku, archived, retry+1);
      }
    }
  }

  @Trace(dispatcher=true)
  @Override
  @Async
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void hideItemSkuByOOSDate(String storeId, String requestId, int batchSize) {
    int daysToCheckFor = this.applicationProperties.getDaysToCheckOOSDateFor();
    LOGGER.info("Hiding itemSku which are oos for more than : {} days", daysToCheckFor);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -(daysToCheckFor));
    Pageable pageable = PageRequest.of(0, 100);
    Page<PbpStockAlert> stockAlerts;
    Map<String, String> businessPartnerTypeMapping = new HashMap<>();
    Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
    int successCount = 0;
    try{
      do {
        stockAlerts = this.productStockAlertRepository
            .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(storeId, calendar.getTime(),
                pageable);
        int failureCount = 0;
        List<String> distinctBusinessPartnerCodes =
            stockAlerts.getContent().stream().map(stockAlert -> stockAlert.getBusinessPartnerCode()).distinct()
                .collect(Collectors.toList());
        filterBusinessPartnerCodes(distinctBusinessPartnerCodes, businessPartnerTypeMapping, profileResponseMap);
        for(PbpStockAlert pbpStockAlert : stockAlerts.getContent()) {
          try{
            if(successCount < batchSize) {
              if (distinctBusinessPartnerCodes.contains(pbpStockAlert.getBusinessPartnerCode())) {
                ItemViewConfigRequest request = new ItemViewConfigRequest();
                request.setBuyable(false);
                request.setDiscoverable(false);
                boolean itemViewConfigUpdated =
                    this.productLevel3Service.updateItemViewConfigToHideItemSku(request, pbpStockAlert.getGdnSku(),
                        StringUtils.EMPTY);
                if (itemViewConfigUpdated) {
                  successCount++;
                }
              }
            }
          } catch (Exception e) {
            failureCount++;
            LOGGER.error("Exception caught while hiding itemSku : {}", pbpStockAlert.getGdnSku(), e);
          }
        }
        LOGGER.info("Number of - successfully hidden : {}, failed : {}", successCount,
            failureCount);
        pageable = stockAlerts.nextPageable();
      } while (stockAlerts.hasNext() && successCount < batchSize);

    } catch (Exception e) {
      LOGGER.error("Error while hiding itemSkus", e);
    }
  }

  private void filterBusinessPartnerCodes(List<String> distinctBusinessPartnerCodes,
      Map<String, String> businessPartnerTypeMapping, Map<String, ProfileResponse> profileResponseMap)
      throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    Iterator<String> iter = distinctBusinessPartnerCodes.iterator();
    while (iter.hasNext()) {
      String distinctBusinessPartnerCode = iter.next();
      if (!businessPartnerTypeMapping.containsKey(distinctBusinessPartnerCode)) {
        profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(distinctBusinessPartnerCode);
        if (CM_MERCHANT.equals(profileResponse.getCompany().getMerchantType()) && !profileResponse.getCompany()
            .isOfflineToOnlineFlag()) {
          profileResponseMap.put(distinctBusinessPartnerCode, profileResponse);
          businessPartnerTypeMapping.put(distinctBusinessPartnerCode, CM_MERCHANT);
        } else {
          iter.remove();
          businessPartnerTypeMapping.put(distinctBusinessPartnerCode, NON_CM_MERCHANT);
        }
      } else {
        if (!CM_MERCHANT.equals(businessPartnerTypeMapping.get(distinctBusinessPartnerCode))) {
          iter.remove();
        }
      }
    }
  }

  @Override
  public void deleteProductStockAlertByStoreIdAndProductId(String storeId, String productId){
    productStockAlertRepository.deleteByStoreIdAndProductId(storeId, productId);
  }
}
