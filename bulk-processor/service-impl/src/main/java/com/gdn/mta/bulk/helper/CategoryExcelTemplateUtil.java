package com.gdn.mta.bulk.helper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.openxml4j.opc.PackagingURIHelper;
import org.apache.xmlbeans.XmlException;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTCell;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTRow;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTSheetData;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTWorksheet;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.STCellType;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.WorksheetDocument;

import com.gdn.partners.bulk.util.Constant;

public class CategoryExcelTemplateUtil {

  private static final String SHARED_STRING = "/xl/sharedStrings.xml";
  private static final String BRAND_SHEET = "/xl/worksheets/sheet5.xml";
  private static final int BRAND_SHEET_COLUMN1 = 0;

  public static Map<String, byte[]> isUploadTemplateExist(String templateDirectory,
    String categoryUploadTemplateFile,
      String categoryUploadTemplateFileEnglish, String categoryBaseTemplateFile, String categoryBaseTemplateFileEnglish) throws IOException {
    File destinationFile;
    Map<String,byte[]> map = new HashMap<>();
    File sourceFile = new File(templateDirectory + categoryBaseTemplateFileEnglish);
    destinationFile = new File(templateDirectory + categoryUploadTemplateFileEnglish);
    map.put(Constant.CATEGORY_UPLOAD_TEMPLATE_EN,FileUtils.readFileToByteArray(destinationFile));
    if (!destinationFile.exists()) {
      FileUtils.copyFile(sourceFile, destinationFile);
    }
    sourceFile = new File(templateDirectory + categoryBaseTemplateFile);
    destinationFile = new File(templateDirectory + categoryUploadTemplateFile);
    map.put(Constant.CATEGORY_UPLOAD_TEMPLATE,FileUtils.readFileToByteArray(destinationFile));
    if (!destinationFile.exists()) {
      FileUtils.copyFile(sourceFile, destinationFile);
    }
    return map;
  }

  public static byte[] regenerateCategoryTemplateBrandValuesSheet(List<String> allActiveBrand,
    byte[] fileByte)
      throws IOException, InvalidFormatException, XmlException {
    try (InputStream inputStream = new ByteArrayInputStream(fileByte)) {
      OPCPackage opcpackage = OPCPackage.open(inputStream);
      PackagePart sheetPart = opcpackage.getPart(PackagingURIHelper.createPartName(BRAND_SHEET));
      WorksheetDocument worksheetDocument = WorksheetDocument.Factory.parse(sheetPart.getInputStream());
      CTWorksheet worksheet = worksheetDocument.getWorksheet();
      CTSheetData sheetData = worksheet.getSheetData();
      sheetData.setNil();
      setBrandHeaders(sheetData);
      for (String brand : allActiveBrand) {
        CTRow row = sheetData.addNewRow();
        CTCell categoryNameColumn = row.insertNewC(BRAND_SHEET_COLUMN1);
        categoryNameColumn.setT(STCellType.STR);
        categoryNameColumn.setV(brand);
      }
      try (ByteArrayOutputStream bos = new ByteArrayOutputStream(); OutputStream outputStream = sheetPart.getOutputStream()) {
        bos.write(worksheetDocument.xmlText().getBytes(StandardCharsets.UTF_8));
        outputStream.write(bos.toByteArray());
      }
      try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
        opcpackage.save(bos);
        return bos.toByteArray();
      }
    }
  }
  private static void setBrandHeaders(CTSheetData sheetData) {
    CTRow row = sheetData.addNewRow();
    CTCell HeaderCell = row.insertNewC(BRAND_SHEET_COLUMN1);
    HeaderCell.setT(STCellType.STR);
    HeaderCell.setV(Constant.ATTRIBUTE_NAME_BRAND);
  }
}
