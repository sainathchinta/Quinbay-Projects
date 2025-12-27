package com.gdn.partners.pcu.external.web.controller.util;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.gdn.partners.core.security.exception.UnauthorizedException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.merchant.service.utils.FileValidator;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ExcelTemplateUtil {

  private static final String COMMA = ", ";
  private static final String DOT = ".";
  private static final String XLS = ".xls";
  private static final String XLSX = ".xlsx";
  private static final String XLSM = ".xlsm";
  private static final String ZIP = ".zip";

  /**
   * Return is multipartFile is xls or xlsx
   *
   * @param multipartFile
   * @return
   * @throws IOException
   */
  public static boolean isInExcelMimeType(MultipartFile multipartFile) throws IOException {
    return !(!FileValidator.checkFileByType(multipartFile.getBytes(), FileValidator.XLS)
        && !FileValidator.checkFileByType(multipartFile.getBytes(), FileValidator.XLSX));
  }

  public static String updateTemplateBulkUpload(MultipartFile request, String username,
    boolean gcsEnabled) throws Exception {
    List<String> accessibilties = Arrays.asList(Credential.getAccessibilities());
    if (!accessibilties.contains(Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION)) {
      throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    String excelFileName = null;
    String zipFileName = null;
    if (request.getOriginalFilename().substring(request.getOriginalFilename().lastIndexOf(DOT)).equals(XLSX) || request
        .getOriginalFilename().substring(request.getOriginalFilename().lastIndexOf(DOT)).equals(XLSM)) {
      excelFileName = request.getOriginalFilename();
    } else if (request.getOriginalFilename().substring(request.getOriginalFilename().lastIndexOf(DOT)).equals(ZIP)) {
      zipFileName = request.getOriginalFilename();
    } else {
      log.error("Uploading template of invalid format: {}", request.getOriginalFilename());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.EXCEL_ZIP_FILE_TYPE_INVALID);
    }

    log.info("Uploading the file at controller. File name : {}", excelFileName + COMMA + zipFileName);
    if (!gcsEnabled) {
      File baseDirectory = new File(Constants.DATA_BASE_DIR);
      if (!baseDirectory.isDirectory()) {
        baseDirectory.mkdir();
      }
      File file = new File(Constants.DATA_BASE_DIR + username + Constants.ROOT);
      if (!file.isDirectory()) {
        file.mkdir();
      }
      file = new File(Constants.DATA_BASE_DIR + username + Constants.ROOT + request.getOriginalFilename());
      log.info("Transferring the file to destination : {}", file.getAbsolutePath());
      request.transferTo(file);
      if (StringUtils.isNotBlank(excelFileName) && !validateExcelTemplate(excelFileName, file)) {
        log.error("Uploading an old template with filename : {}", excelFileName);
        file.delete();
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_TEMPLATE);
      }
    }
    return excelFileName + COMMA + zipFileName;
  }

  public static String updateSubjectToVatTemplate(MultipartFile request, String vatFilePath, String bulkProcessCode) throws Exception {
    String excelFileName;
    if (request.getOriginalFilename().substring(request.getOriginalFilename().lastIndexOf(DOT)).equals(XLSX)) {
      excelFileName = request.getOriginalFilename();
    } else {
      log.error("Uploading template of invalid format: {}", request.getOriginalFilename());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE);
    }
    log.info("Uploading the file at controller. File name : {}", excelFileName);
    excelFileName = vatFilePath + Constants.ROOT + bulkProcessCode + Constants.ROOT;
    File file = new File(excelFileName);
    if (!file.exists()) {
      file.mkdirs();
    }
    excelFileName = excelFileName + bulkProcessCode + XLSX;
    file = new File(excelFileName);
    log.info("Transferring the file to destination : {}", file.getAbsolutePath());
    request.transferTo(file);
    return excelFileName;
  }


  private static boolean validateExcelTemplate(String excelFilename, File request) throws Exception {
    boolean valid = false;
    if (excelFilename.endsWith(XLSX)) {
      XSSFWorkbook workbook = new XSSFWorkbook(request);
      if (Objects.nonNull(workbook.getSheet(Constants.CATEGORY_SHEET)) || Objects.nonNull(
          workbook.getSheet(Constants.BULK_UPSERT_SHEET))) {
        valid = true;
      }
    } else if (excelFilename.endsWith(XLSM)) {
      try (OPCPackage opcpackage = OPCPackage.open(request)) {
        List<PackagePart> sheetParts = opcpackage.getPartsByName(Constants.CATEGORY_PATTERN);
        if (CollectionUtils.isNotEmpty(sheetParts)) {
          valid = true;
        }
      }
    }
    return valid;
  }

  public static String deleteBulkTemplate(String fileName, String username) throws IOException {
    String excelFileName = null;
    String zipFileName = null;
    if (!StringUtils.isBlank(fileName)) {
      if (fileName.substring(fileName.lastIndexOf(DOT)).equals(XLS) ||
          fileName.substring(fileName.lastIndexOf(DOT)).equals(XLSX) ||
          fileName.substring(fileName.lastIndexOf(DOT)).equals(XLSM)) {
        excelFileName = fileName;
      } else if (fileName.substring(fileName.lastIndexOf(DOT)).equals(ZIP)) {
        zipFileName = fileName;
      }
      File file = new File(Constants.DATA_BASE_DIR + username + Constants.ROOT + fileName);
      file.delete();
    }
    return excelFileName + COMMA + zipFileName;
  }

  public static void deleteFolder(String username) throws IOException {
    File file = new File(Constants.DATA_BASE_DIR + username + Constants.ROOT);
    if(file.exists()){
      FileUtils.cleanDirectory(file);
    }
  }
}
