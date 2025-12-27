package com.gdn.mta.bulk.service.download;

import java.io.ByteArrayOutputStream;
import java.util.List;
import org.apache.poi.ss.usermodel.Workbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.BulkDownloadErrorCode;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.FileStorageService;

/**
 * Created by keshashah on 14/11/16.
 */
@Component
public class BulkProcessFileGeneration {
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProcessFileGeneration.class);

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${bulk.update.template.column.width}")
  private int bulkUpdateTemplateColumnWidth;

  public String generateFileFromResponse(BulkDownloadRequest request, BulkDataResponse response,
      BulkProcessHelper helper) throws BulkDownloadException {
    try {
      if (FileType.CSV.name().equals(request.getFileType().name())) {
        BulkCsvModel csvHeadersMap = helper.getCsvHeadersMap(request);
        byte[] bytes = helper.generateCsvFile(csvHeadersMap, response);
        return fileStorageService.generateFile(request, bytes);
      } else if (FileType.XLSX.name().equals(request.getFileType().name())) {
        List<String> headerList = helper.getHeaderList(response);
        Workbook dataSheet =
            helper.generateDataSheet(headerList, helper.getRowData(response), bulkUpdateTemplateColumnWidth);
        Workbook finalWorkbook = helper.modifyWorkbook(dataSheet, response);
        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
          finalWorkbook.write(byteArrayOutputStream);
          return fileStorageService.generateFile(request, byteArrayOutputStream.toByteArray());
        }
      }
    } catch (Exception e) {
      LOGGER.error("Bulk Download: Error generating bulk download file for request {}", request, e);
      throw new BulkDownloadException(BulkDownloadErrorCode.ERROR_GENERATING_FILE.toString(),
          BulkDownloadErrorCode.ERROR_GENERATING_FILE.getErrorMessage());
    }
    return null;
  }

  public BulkInternalProcessDTO generateFileResponse(BulkDownloadRequest request, BulkDataResponse response,
      BulkProcessHelper helper) throws Exception {
    List<String> headerList = helper.getHeaderList(response);
    Workbook dataSheet =
        helper.generateDataSheet(headerList, helper.getRowData(response), bulkUpdateTemplateColumnWidth);
    Workbook workbook = helper.modifyWorkbook(dataSheet, response);
    String helperDirectoryPath = helper.getDirectory(request);
    String helperFilePath = helper.getFilePath(helperDirectoryPath, request.getFilename());
    BulkInternalProcessDTO bulkInternalProcessDTO =
        BulkInternalProcessDTO.builder().filepath(helperFilePath).directoryPath(helperDirectoryPath).finalWorkbook(workbook)
            .build();
    return bulkInternalProcessDTO;
  }
}
