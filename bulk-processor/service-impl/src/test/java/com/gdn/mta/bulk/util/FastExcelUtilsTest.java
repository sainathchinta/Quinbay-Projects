package com.gdn.mta.bulk.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.models.ColumnConfig;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.models.SheetConfig;
import org.dhatim.fastexcel.reader.Cell;
import org.dhatim.fastexcel.reader.ReadableWorkbook;
import org.dhatim.fastexcel.reader.Row;
import org.dhatim.fastexcel.reader.Sheet;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipFile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class FastExcelUtilsTest {

  private static final String ZIP_FILE_NAME = "zipFileName";
  private static final String FILE_NAME = "zipFileName";

  SheetConfig config;
  Map<String, String> sourceColumnMap;
  Map<String, Boolean> mandatoryMap;
  Map<String, StringBuilder> validationErrors;
  Map<String, List<Map<String, String>>> basicInfoMap;
  Map<String, List<Map<String, String>>> mediaInfoMap;
  Map<String, List<Map<String, String>>> shippingInfoMap;
  Map<String, List<Map<String, String>>> salesInfoMap;
  PlatformConfig platformConfig;

  @BeforeEach
  void setup() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    config = new SheetConfig();
    config.setHeaderRowIndex(0);
    config.setDataStartRowIndex(1);
    config.setMaxColumns(5);
    config.setMaxRows(2);
    config.setColumns(Arrays.asList(new ColumnConfig("Col1", new ArrayList<>(), "Dest1", true),
      new ColumnConfig("Col2", new ArrayList<>(), "Dest2", false)));

    sourceColumnMap = new HashMap<>();
    mandatoryMap = new HashMap<>();
    validationErrors = new HashMap<>();
    basicInfoMap = new HashMap<>();
    mediaInfoMap = new HashMap<>();
    shippingInfoMap = new HashMap<>();
    salesInfoMap = new HashMap<>();
    platformConfig = new ObjectMapper().readValue("[\n" + "  {\n"
        + "    \"platform\": \"shopee\",\n" + "    \"joinKey\": \"Kode Produk\",\n"
        + "    \"sheetConfigs\": [\n" + "      {\n" + "        \"sheetType\": \"basicInfo\",\n"
        + "        \"sheetIndex\": 0,\n" + "        \"maxRows\": 10000,\n"
        + "        \"maxColumns\": 50,\n" + "        \"headerRowIndex\": 2,\n"
        + "        \"dataStartRowIndex\": 6,\n" + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Nama Produk\",\n"
        + "            \"destinationColumn\": \"Nama Produk*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n"
        + "            \"sourceColumn\": \"Deskripsi Produk\",\n"
        + "            \"destinationColumn\": \"Deskripsi*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"media\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 5000,\n" + "        \"maxColumns\": 2000,\n"
        + "        \"headerRowIndex\": 2,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Kategori\",\n"
        + "            \"destinationColumn\": \"External category\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Sampul\",\n"
        + "            \"destinationColumn\": \"Foto-1*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 1\",\n"
        + "            \"destinationColumn\": \"Foto-2\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 2\",\n"
        + "            \"destinationColumn\": \"Foto-3\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 3\",\n"
        + "            \"destinationColumn\": \"Foto-4\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 4\",\n"
        + "            \"destinationColumn\": \"Foto-5\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 5\",\n"
        + "            \"destinationColumn\": \"Foto-6\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Foto Produk 6\",\n"
        + "            \"destinationColumn\": \"Foto-7\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": false\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"shipping\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 30000,\n" + "        \"maxColumns\": 200,\n"
        + "        \"headerRowIndex\": 3,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Berat Produk/g\",\n"
        + "            \"destinationColumn\": \"Berat (gram)*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Panjang\",\n"
        + "            \"destinationColumn\": \"Panjang (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Lebar\",\n"
        + "            \"destinationColumn\": \"Lebar (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Tinggi\",\n"
        + "            \"destinationColumn\": \"Tinggi (cm)*\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      },\n" + "      {\n"
        + "        \"sheetType\": \"sales\",\n" + "        \"sheetIndex\": 0,\n"
        + "        \"maxRows\": 20000,\n" + "        \"maxColumns\": 30,\n"
        + "        \"headerRowIndex\": 2,\n" + "        \"dataStartRowIndex\": 6,\n"
        + "        \"columns\": [\n" + "          {\n"
        + "            \"sourceColumn\": \"Kode Produk\",\n"
        + "            \"destinationColumn\": \"productId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Kode Variasi\",\n"
        + "            \"destinationColumn\": \"variantId\",\n"
        + "            \"dataType\": \"STRING\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Nama Variasi\",\n"
        + "            \"destinationColumn\": \"Variasi\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"SKU\",\n"
        + "            \"destinationColumn\": \"Seller SKU\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": false\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Harga\",\n"
        + "            \"destinationColumn\": \"Harga Penjualan (Rp)*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          },\n" + "          {\n" + "            \"sourceColumn\": \"Stok\",\n"
        + "            \"destinationColumn\": \"Available Stock*\",\n"
        + "            \"dataType\": \"NUMBER\",\n" + "            \"mandatory\": true\n"
        + "          }\n" + "        ]\n" + "      }\n" + "    ]\n" + "  }\n" + "]",
      new TypeReference<List<PlatformConfig>>() {
      }).get(0);
  }

  @Test
  void testValidateHeadersAndMaxCols_headerNull() {
    Exception e = assertThrows(ValidationException.class,
      () -> FastExcelUtils.validateHeadersAndMaxCols(config, "file.xlsx", null, ZIP_FILE_NAME));
    assertTrue(((ValidationException) e).getErrorMessage().contains("tidak ditemukan"));
  }

  @Test
  void testValidateHeadersAndMaxCols_exceedMaxCols() {
    Row row = mock(Row.class);
    when(row.getCellCount()).thenReturn(10);
    Exception e = assertThrows(ValidationException.class,
      () -> FastExcelUtils.validateHeadersAndMaxCols(config, "file.xlsx", row, ZIP_FILE_NAME));
    assertTrue(((ValidationException) e).getErrorMessage().contains("jumlah maksimum"));
  }

  @Test
  void testGetHeaderIndexMap() {
    Row row = mock(Row.class);
    when(row.getCellCount()).thenReturn(2);
    when(row.getCellText(0)).thenReturn("Col1 ");
    when(row.getCellText(1)).thenReturn("Col2");
    Map<String, Integer> map = FastExcelUtils.getHeaderIndexMap(row);
    assertEquals(2, map.size());
    assertEquals(0, map.get("col1"));
    assertEquals(1, map.get("col2"));
  }

  @Test
  void testProcessEachRowPerSheet_blankJoinKey_skipsRow() {
    Row row = mock(Row.class);
    when(row.getCellText(anyInt())).thenReturn("  ");
    Iterator<Row> iterator = Arrays.asList(row).iterator();
    FastExcelUtils.processEachRowPerSheet(ZIP_FILE_NAME, FILE_NAME, mandatoryMap, validationErrors,
      config,
      iterator, 0, 0, 0,
      new HashMap<>(), new HashMap<>());
    assertTrue(validationErrors.isEmpty());
  }

  @Test
  void testProcessEachRowPerSheet_exceedMaxRows_throws() {
    Row row1 = mock(Row.class);
    Row row2 = mock(Row.class);
    Row row3 = mock(Row.class);
    when(row1.getCellText(anyInt())).thenReturn("key1");
    when(row2.getCellText(anyInt())).thenReturn("key2");
    when(row3.getCellText(anyInt())).thenReturn("key3");
    Iterator<Row> iterator = Arrays.asList(row1, row2, row3).iterator();
    Exception e = assertThrows(ValidationException.class,
      () -> FastExcelUtils.processEachRowPerSheet(ZIP_FILE_NAME, FILE_NAME, mandatoryMap, validationErrors,
        config, iterator,
        0, 0, 0, new HashMap<>(), new HashMap<>()));
    assertTrue(((ValidationException) e).getErrorMessage().contains("jumlah maksimum"));
  }

  @Test
  void testProcessEachFileForExternalUpload_success1() throws Exception {
    Row header = mock(Row.class);
    when(header.getCellCount()).thenReturn(2);
    when(header.getCellText(0)).thenReturn("Col1");
    when(header.getCellText(1)).thenReturn("Col2");

    Row dataRow = mock(Row.class);
    when(dataRow.getCellText(0)).thenReturn("key1");
    when(dataRow.getCellText(1)).thenReturn("value");

    Sheet sheet = mock(Sheet.class);
    when(sheet.openStream()).thenReturn(Stream.of(header, dataRow));
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    when(workbook.getSheet(anyInt())).thenReturn(Optional.of(sheet));

    FastExcelUtils.processEachFileForExternalUpload("zipFileName", sourceColumnMap, mandatoryMap,
      validationErrors, basicInfoMap, mediaInfoMap, shippingInfoMap, salesInfoMap, workbook, config,
      "file.xlsx", "Col1", "basicInfo");

    assertFalse(basicInfoMap.containsKey("key1"));
  }
  @Test
  void testProcessEachZipFileForExternalUpload_withActualZipFile() throws Exception {
    // Get resource file path
    File resourceZip = new File("src/test/resources/externalCreationUpload/NAPOCUT.zip");
    // Optionally, copy to temp folder if you want
    File tempZip = File.createTempFile("testzip", ".zip");
    Files.copy(resourceZip.toPath(), tempZip.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);

    try (ZipFile zipFile = new ZipFile(tempZip)) {
      // Prepare your maps and configs as per your actual logic
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      excelFileNameToTypeMap.put("Basic Shopee.xlsx", "basicInfo");
      excelFileNameToTypeMap.put("Info Sales Shopee.xlsx","sales");
      excelFileNameToTypeMap.put("info shipping napocut.xlsx", "shipping");
      excelFileNameToTypeMap.put("Media Shopee.xlsx", "media");
      Map<String, SheetConfig> sheetConfigMap = platformConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      Map<String, String> sourceColumnMap = new HashMap<>();
      Map<String, Boolean> mandatoryMap = new HashMap<>();
      Map<String, StringBuilder> validationErrors = new HashMap<>();
      Map<String, List<Map<String, String>>> basicInfoMap =new HashMap<>();
      Map<String, List<Map<String, String>>> mediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> salesInfoMap = new HashMap<>();

      // Call the actual method
      FastExcelUtils.processEachZipFileForExternalUpload(ZIP_FILE_NAME, zipFile, excelFileNameToTypeMap, sheetConfigMap,
        platformConfig, sourceColumnMap, mandatoryMap, validationErrors, basicInfoMap,
        mediaInfoMap, shippingInfoMap, salesInfoMap);

      // Assert what you expect in the output maps
      assertFalse(basicInfoMap.containsKey("expectedKey")); // Example
      // ... other assertions
    } finally {
      Files.deleteIfExists(tempZip.toPath());
    }
  }

  @Test
  void testProcessEachZipFileForExternalUpload_withActualZipFile2() throws Exception {
    // Get resource file path
    File resourceZip = new File("src/test/resources/externalCreationUpload/Basic Shopee.zip");
    // Optionally, copy to temp folder if you want
    String uniquePrefix = "testzip-" + UUID.randomUUID();
    File tempZip = File.createTempFile(uniquePrefix, ".zip");
    Files.copy(resourceZip.toPath(), tempZip.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
    platformConfig.getSheetConfigs().get(1).setSheetIndex(2);
    try (ZipFile zipFile = new ZipFile(tempZip)) {
      // Prepare your maps and configs as per your actual logic
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      excelFileNameToTypeMap.put("Basic Shopee.xlsx", "basicInfo");
      excelFileNameToTypeMap.put("Info Sales Shopee.xlsx","sales");
      excelFileNameToTypeMap.put("info shipping napocut.xlsx", "shipping");
      excelFileNameToTypeMap.put("Media Shopee.xlsx", "media");
      Map<String, SheetConfig> sheetConfigMap = platformConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      Map<String, String> sourceColumnMap = new HashMap<>();
      Map<String, Boolean> mandatoryMap = new HashMap<>();
      Map<String, StringBuilder> validationErrors = new HashMap<>();
      Map<String, List<Map<String, String>>> basicInfoMap =new HashMap<>();
      Map<String, List<Map<String, String>>> mediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> salesInfoMap = new HashMap<>();

      // Call the actual method
      assertThrows(ValidationException.class, () -> {
        FastExcelUtils.processEachZipFileForExternalUpload(ZIP_FILE_NAME, zipFile, excelFileNameToTypeMap, sheetConfigMap,
          platformConfig, sourceColumnMap, mandatoryMap, validationErrors,
          basicInfoMap, mediaInfoMap, shippingInfoMap, salesInfoMap
        );
      });
      // Assert what you expect in the output maps
      assertFalse(basicInfoMap.containsKey("expectedKey")); // Example
      // ... other assertions
    } finally {
      Files.deleteIfExists(tempZip.toPath());
    }
  }

  @Test
  void testProcessEachZipFileForExternalUpload_withActualZipFile3() throws Exception {
    // Get resource file path
    File resourceZip = new File("src/test/resources/externalCreationUpload/NAPOCUT.zip");
    // Optionally, copy to temp folder if you want
    String uniquePrefix = "testzip-" + UUID.randomUUID();
    File tempZip = File.createTempFile(uniquePrefix, ".zip");
    Files.copy(resourceZip.toPath(), tempZip.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
    platformConfig.getSheetConfigs().get(1).setSheetIndex(2);
    try (ZipFile zipFile = new ZipFile(tempZip)) {
      // Prepare your maps and configs as per your actual logic
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      excelFileNameToTypeMap.put("Basic Shopee.xlsx", "basicInfo");
      excelFileNameToTypeMap.put("Info Sales Shopee.xlsx","sales");
      excelFileNameToTypeMap.put("info shipping napocut.xlsx", "shipping");
      excelFileNameToTypeMap.put("Media Shopee.xlsx", "media");
      Map<String, SheetConfig> sheetConfigMap = platformConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      Map<String, String> sourceColumnMap = new HashMap<>();
      Map<String, Boolean> mandatoryMap = new HashMap<>();
      Map<String, StringBuilder> validationErrors = new HashMap<>();
      Map<String, List<Map<String, String>>> basicInfoMap =new HashMap<>();
      Map<String, List<Map<String, String>>> mediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> salesInfoMap = new HashMap<>();

      // Call the actual method
      assertThrows(ValidationException.class, () -> {
        FastExcelUtils.processEachZipFileForExternalUpload(ZIP_FILE_NAME, zipFile, excelFileNameToTypeMap, sheetConfigMap,
          platformConfig, sourceColumnMap, mandatoryMap, validationErrors,
          basicInfoMap, mediaInfoMap, shippingInfoMap, salesInfoMap
        );
      });

      // Assert what you expect in the output maps
      assertFalse(basicInfoMap.containsKey("expectedKey")); // Example
      // ... other assertions
    } finally {
      Files.deleteIfExists(tempZip.toPath());
    }
  }

  @Test
  void testProcessEachZipFileForExternalUpload_withActualZipFil3() throws Exception {
    // Get resource file path
    File resourceZip = new File("src/test/resources/externalCreationUpload/Basic Shopee1.zip");
    // Optionally, copy to temp folder if you want
    File tempZip = File.createTempFile("testzip", ".zip");
    Files.copy(resourceZip.toPath(), tempZip.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);

    try (ZipFile zipFile = new ZipFile(tempZip)) {
      // Prepare your maps and configs as per your actual logic
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      excelFileNameToTypeMap.put("Basic Shopee.xlsx", "basicInfo");
      excelFileNameToTypeMap.put("Info Sales Shopee.xlsx","sales");
      excelFileNameToTypeMap.put("info shipping napocut.xlsx", "shipping");
      excelFileNameToTypeMap.put("Media Shopee.xlsx", "media");
      Map<String, SheetConfig> sheetConfigMap = platformConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      Map<String, String> sourceColumnMap = new HashMap<>();
      Map<String, Boolean> mandatoryMap = new HashMap<>();
      Map<String, StringBuilder> validationErrors = new HashMap<>();
      Map<String, List<Map<String, String>>> basicInfoMap =new HashMap<>();
      Map<String, List<Map<String, String>>> mediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> salesInfoMap = new HashMap<>();

      // Call the actual method
      assertThrows(ValidationException.class, () -> {
        FastExcelUtils.processEachZipFileForExternalUpload(ZIP_FILE_NAME, zipFile, excelFileNameToTypeMap, sheetConfigMap,
          platformConfig, sourceColumnMap, mandatoryMap, validationErrors,
          basicInfoMap, mediaInfoMap, shippingInfoMap, salesInfoMap
        );
      });

      // Assert what you expect in the output maps
      assertFalse(basicInfoMap.containsKey("expectedKey")); // Example
      // ... other assertions
    } finally {
      Files.deleteIfExists(tempZip.toPath());
    }
  }

  @Test
  void testProcessEachZipFileForExternalUpload_withActualZipFil4() throws Exception {
    // Get resource file path
    File resourceZip = new File("src/test/resources/externalCreationUpload/NAPOCUT.zip");
    // Optionally, copy to temp folder if you want
    File tempZip = File.createTempFile("testzip", ".zip");
    Files.copy(resourceZip.toPath(), tempZip.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
    platformConfig.getSheetConfigs().get(0).setMaxRows(7);
    try (ZipFile zipFile = new ZipFile(tempZip)) {
      // Prepare your maps and configs as per your actual logic
      Map<String, String> excelFileNameToTypeMap = new HashMap<>();
      excelFileNameToTypeMap.put("Basic Shopee.xlsx", "basicInfo");
      excelFileNameToTypeMap.put("Info Sales Shopee.xlsx","sales");
      excelFileNameToTypeMap.put("info shipping napocut.xlsx", "shipping");
      excelFileNameToTypeMap.put("Media Shopee.xlsx", "media");
      Map<String, SheetConfig> sheetConfigMap = platformConfig.getSheetConfigs().stream()
        .collect(Collectors.toMap(SheetConfig::getSheetType, sc -> sc));
      Map<String, String> sourceColumnMap = new HashMap<>();
      Map<String, Boolean> mandatoryMap = new HashMap<>();
      Map<String, StringBuilder> validationErrors = new HashMap<>();
      Map<String, List<Map<String, String>>> basicInfoMap =new HashMap<>();
      Map<String, List<Map<String, String>>> mediaInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
      Map<String, List<Map<String, String>>> salesInfoMap = new HashMap<>();

      // Call the actual method
      assertThrows(ValidationException.class, () -> {
        FastExcelUtils.processEachZipFileForExternalUpload(ZIP_FILE_NAME, zipFile, excelFileNameToTypeMap, sheetConfigMap,
          platformConfig, sourceColumnMap, mandatoryMap, validationErrors,
          basicInfoMap, mediaInfoMap, shippingInfoMap, salesInfoMap
        );
      });

      // Assert what you expect in the output maps
      assertFalse(basicInfoMap.containsKey("expectedKey")); // Example
      // ... other assertions
    } finally {
      Files.deleteIfExists(tempZip.toPath());
    }
  }

  @Test
  void testShouldProcessRow_hasNextAndCurrentLessThanTarget_returnsTrue() {
    Iterator<Integer> iterator = Arrays.asList(1, 2).iterator();
    int currentRowNum = 2;
    int targetRowIndex = 5;
    assertTrue(FastExcelUtils.shouldProcessRow(targetRowIndex, iterator, currentRowNum));
  }

  @Test
  void testShouldProcessRow_noNext_returnsFalse() {
    Iterator<Integer> iterator = List.<Integer>of().iterator();
    int currentRowNum = 2;
    int targetRowIndex = 5;
    assertFalse(FastExcelUtils.shouldProcessRow(targetRowIndex, iterator, currentRowNum));
  }

  @Test
  void testShouldProcessRow_currentGreaterThanTarget_returnsFalse() {
    Iterator<Integer> iterator = Arrays.asList(1).iterator();
    int currentRowNum = 6;
    int targetRowIndex = 5;
    assertFalse(FastExcelUtils.shouldProcessRow(targetRowIndex, iterator, currentRowNum));
  }

  @Test
  public void testReadHeaders_SkipRowsAndReadHeaders() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row skipRow = mock(Row.class);
    Row headerRow = mock(Row.class);
    Cell h1 = mock(Cell.class);
    Cell h2 = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(skipRow, headerRow));
    when(headerRow.getCellCount()).thenReturn(2);
    when(headerRow.getCell(0)).thenReturn(h1);
    when(headerRow.getCell(1)).thenReturn(h2);
    when(h1.getRawValue()).thenReturn("H1");
    when(h2.getRawValue()).thenReturn("H2");

    Map<Integer, String> headers = FastExcelUtils.readHeaders(workbook, 1, 0, "BPCODE");
    Assertions.assertEquals(2, headers.size());
    Assertions.assertEquals("H1", headers.get(0));
    Assertions.assertEquals("H2", headers.get(1));
  }

  @Test
  public void testReadHeaders_NullSheetAndNoHeaderRow() throws IOException {
    ReadableWorkbook workbook1 = mock(ReadableWorkbook.class);
    when(workbook1.getFirstSheet()).thenReturn(null);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> FastExcelUtils.readHeaders(workbook1, 0, 0, "BP1"));

    ReadableWorkbook workbook2 = mock(ReadableWorkbook.class);
    Sheet sheet2 = mock(Sheet.class);
    when(workbook2.getFirstSheet()).thenReturn(sheet2);
    when(sheet2.openStream()).thenReturn(Stream.empty());
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> FastExcelUtils.readHeaders(workbook2, 0, 0, "BP2"));
  }

  @Test
  public void testReadDataRows_SkipStartAndHeaderEmptyContinue() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row headerLike = mock(Row.class);
    Row dataRow = mock(Row.class);
    Cell dataCell0 = mock(Cell.class);
    Cell dataCell1 = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(headerLike, dataRow));
    when(dataRow.getCellCount()).thenReturn(2);
    when(dataRow.getCell(0)).thenReturn(dataCell0);
    when(dataRow.getCell(1)).thenReturn(dataCell1);
    when(dataCell0.getRawValue()).thenReturn("VAL1");
    when(dataCell1.getRawValue()).thenReturn("IGNORED");
    when(dataRow.getRowNum()).thenReturn(5);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "ColA");
    headers.put(1, "");

    // condition validateBulkMaxNumberOfRows = false -> branch not entered
    List<Map<String, String>> result = FastExcelUtils.readDataRows(workbook, headers, 1, 10, false, "BPX");
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals("VAL1", result.get(0).get("ColA"));
    Assertions.assertEquals(String.valueOf(5), result.get(0).get("RowNumber"));
  }

  @Test
  public void testReadDataRows_AllBlankSkipped() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row row = mock(Row.class);
    Cell cell = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(row));
    when(row.getCellCount()).thenReturn(1);
    when(row.getCell(0)).thenReturn(cell);
    when(cell.getRawValue()).thenReturn("");
    when(row.getRowNum()).thenReturn(2);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "Header");

    List<Map<String, String>> res = FastExcelUtils.readDataRows(workbook, headers, 0, 10, true, "BPY");
    Assertions.assertTrue(res.isEmpty());
  }

  @Test
  public void testReadDataRows_ExceedsMaxRowsThrows() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row r1 = mock(Row.class);
    Row r2 = mock(Row.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(r1, r2));
    Cell c = mock(Cell.class);
    when(r1.getCellCount()).thenReturn(1);
    when(r2.getCellCount()).thenReturn(1);
    when(r1.getCell(0)).thenReturn(c);
    when(r2.getCell(0)).thenReturn(c);
    when(c.getRawValue()).thenReturn("d");
    when(r1.getRowNum()).thenReturn(1);
    when(r2.getRowNum()).thenReturn(2);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "C");

    // validateBulkMaxNumberOfRows = true and logicalDataRowIndex > bulkMaxNumberOfRows
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
      FastExcelUtils.readDataRows(workbook, headers, 0, 1, true, "BPZ"));
  }

  @Test
  public void testReadDataRows_RowHasFewerCellsThanHeaders() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row dataRow = mock(Row.class);
    Cell dataCell0 = mock(Cell.class);
    Cell dataCell1 = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(dataRow));
    when(dataRow.getCellCount()).thenReturn(2);
    when(dataRow.getCell(0)).thenReturn(dataCell0);
    when(dataRow.getCell(1)).thenReturn(dataCell1);
    when(dataCell0.getRawValue()).thenReturn("VAL1");
    when(dataCell1.getRawValue()).thenReturn("VAL2");
    when(dataRow.getRowNum()).thenReturn(1);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "ColA");
    headers.put(1, "ColB");
    headers.put(2, "ColC");

    List<Map<String, String>> result = FastExcelUtils.readDataRows(workbook, headers, 0, 10, false, "BPX");
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals("VAL1", result.get(0).get("ColA"));
    Assertions.assertEquals("VAL2", result.get(0).get("ColB"));
    Assertions.assertEquals("", result.get(0).get("ColC"));
    Assertions.assertEquals(String.valueOf(1), result.get(0).get("RowNumber"));
  }

  @Test
  public void skipRowsUntilStartIndex_ShouldSkipRows_WhenRowsAvailable() {
    Iterator<String> rowIterator = List.of("r1", "r2", "r3").iterator();
    int result = FastExcelUtils.skipRowsUntilStartIndex(2, 0, (Iterator) rowIterator);
    assertEquals(2, result);
  }

  @Test
  public void skipRowsUntilStartIndex_ShouldStop_WhenNoRowsAvailable() {
    Iterator<String> mockIterator = Mockito.mock(Iterator.class);
    Mockito.when(mockIterator.hasNext()).thenReturn(false);
    int result = FastExcelUtils.skipRowsUntilStartIndex(5, 0, (Iterator) mockIterator);
    assertEquals(0, result);
    Mockito.verify(mockIterator).hasNext();
  }

  @Test
  public void getStringHeaderValue_ShouldReturnEmpty_WhenCellIsNull() {
    String result = FastExcelUtils.getStringHeaderValue(null, false);
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnEmpty_WhenRawValueIsBlank() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("   ");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnEmpty_WhenRawValueIsEmpty() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnDoubleFormat_WhenRawValueIsInteger() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("5");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("5.0", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnDoubleFormat_WhenRawValueIsDecimal() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("5.5");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("5.5", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnDoubleFormat_WhenRawValueIsAlreadyDouble() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("5.0");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("5.0", result);
  }

  @Test
  public void getStringHeaderValue_ShouldReturnRawValue_WhenCellIsNotNull() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("HeaderValue");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, false);
    assertEquals("HeaderValue", result);
    Mockito.verify(mockCell).getRawValue();
  }

  @Test
  public void getStringHeaderValue_ShouldReturnRawValue_WhenCellIsNotNullWithIsStringTrue() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getRawValue()).thenReturn("HeaderValue");
    String result = FastExcelUtils.getStringHeaderValue(mockCell, true);
    assertEquals("HeaderValue", result);
    Mockito.verify(mockCell).getRawValue();
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnEmpty_WhenCellIsNull() {
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(null, false, "Status", java.util.Set.of("Status"));
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnEmpty_WhenCellValueIsBlank() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("   ");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("Status"));
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnEmpty_WhenCellValueIsEmpty() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("Status"));
    assertEquals("", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenStringValueIsTrue() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("TestValue");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, true, "Status", java.util.Set.of("Status"));
    assertEquals("TestValue", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnDouble_WhenColumnMatchesAndStringValueFalse() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("Status"));
    assertEquals("1.0", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnNotMatches() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "OtherColumn", java.util.Set.of("Status"));
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnsToParseAsDoubleIsNull() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", null);
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnsToParseAsDoubleIsEmpty() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of());
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenNumberFormatException() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("NotANumber");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("Status"));
    assertEquals("NotANumber", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnDouble_WhenColumnMatchesCaseInsensitive() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("2");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "status", java.util.Set.of("Status"));
    assertEquals("2.0", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnDouble_WhenColumnMatchesWithWhitespace() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("3");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, " Status ", java.util.Set.of("Status"));
    assertEquals("3.0", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnNameIsBlank() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "   ", java.util.Set.of("Status"));
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnNameIsEmpty() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "", java.util.Set.of("Status"));
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenColumnNameIsNull() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, null, java.util.Set.of("Status"));
    assertEquals("1", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenConfiguredColumnIsBlank() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("Status", "   ", "Other"));
    assertEquals("1.0", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenConfiguredColumnIsEmpty() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "Status", java.util.Set.of("", "Status"));
    assertEquals("1.0", result);
  }

  @Test
  public void getStringHeaderValueForBulkUpdate_ShouldReturnText_WhenMultipleConfiguredColumnsWithBlank() {
    Cell mockCell = Mockito.mock(Cell.class);
    Mockito.when(mockCell.getText()).thenReturn("1");
    String result = FastExcelUtils.getStringHeaderValueForBulkUpdate(mockCell, false, "OtherColumn", java.util.Set.of("   ", "", "Status"));
    assertEquals("1", result);
  }

  @Test
  public void readDataRows_ShouldUseGetStringHeaderValueForBulkUpdate_WhenColumnsToParseAsDoubleProvided() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row dataRow = mock(Row.class);
    Cell statusCell = mock(Cell.class);
    Cell otherCell = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(dataRow));
    when(dataRow.getCellCount()).thenReturn(2);
    when(dataRow.getCell(0)).thenReturn(statusCell);
    when(dataRow.getCell(1)).thenReturn(otherCell);
    when(statusCell.getText()).thenReturn("1");
    when(otherCell.getText()).thenReturn("OtherValue");
    when(dataRow.getRowNum()).thenReturn(1);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "Status");
    headers.put(1, "OtherColumn");

    List<Map<String, String>> result = FastExcelUtils.readDataRows(workbook, headers, 0, 10, false, "BPX", java.util.Set.of("Status"));
    assertEquals(1, result.size());
    assertEquals("1.0", result.get(0).get("Status"));
    assertEquals("OtherValue", result.get(0).get("OtherColumn"));
  }

  @Test
  public void readDataRows_ShouldUseGetStringHeaderValue_WhenColumnsToParseAsDoubleIsNull() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row dataRow = mock(Row.class);
    Cell cell = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(dataRow));
    when(dataRow.getCellCount()).thenReturn(1);
    when(dataRow.getCell(0)).thenReturn(cell);
    when(cell.getRawValue()).thenReturn("Value");
    when(dataRow.getRowNum()).thenReturn(1);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "Column");

    List<Map<String, String>> result = FastExcelUtils.readDataRows(workbook, headers, 0, 10, false, "BPX", null);
    assertEquals(1, result.size());
    assertEquals("Value", result.get(0).get("Column"));
  }

  @Test
  public void readDataRows_ShouldUseGetStringHeaderValue_WhenColumnsToParseAsDoubleIsEmpty() throws Exception {
    ReadableWorkbook workbook = mock(ReadableWorkbook.class);
    Sheet sheet = mock(Sheet.class);
    Row dataRow = mock(Row.class);
    Cell cell = mock(Cell.class);

    when(workbook.getFirstSheet()).thenReturn(sheet);
    when(sheet.openStream()).thenReturn(Stream.of(dataRow));
    when(dataRow.getCellCount()).thenReturn(1);
    when(dataRow.getCell(0)).thenReturn(cell);
    when(cell.getRawValue()).thenReturn("Value");
    when(dataRow.getRowNum()).thenReturn(1);

    Map<Integer, String> headers = new TreeMap<>();
    headers.put(0, "Column");

    List<Map<String, String>> result = FastExcelUtils.readDataRows(workbook, headers, 0, 10, false, "BPX", java.util.Set.of());
    assertEquals(1, result.size());
    assertEquals("Value", result.get(0).get("Column"));
  }
}
