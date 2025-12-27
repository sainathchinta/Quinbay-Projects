package com.gdn.x.product.service.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductScoreRuleConstants;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.ItemScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;
import com.gdn.x.product.rest.web.model.response.MaxScoreAndRuleConfigResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.RuleConfigResponse;
import com.gdn.x.product.service.api.CategoryService;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

public class ProductScoreUtilBeanTest {

  private static final String FILE_PATH = "ProductScoring";
  private static final String IPHONE_JSON = "iphone.json";
  private static final String NO_RECCOMENDED_ATTRIBUTES = "NoRecommendedAttributes.json";
  private static final String NO_REMAINING_ATTRIBUTES = "NoRemainingAttributes.json";
  private static final String RULES_JSON = "Rules.json";
  private static final String RULES1_JSON = "Rules1.json";
  private static final String CATEGORY_CODE = "IP-1000003";
  private static final String CATEGORY_CODE_1 = "MU-1000056";
  private static final String PRODUCT_CODE = "Product code";
  private static final String NAME = "NAME";
  private static final String USP = "USP";
  private static final String DESCRIPTION = "Age table First name Last name Age Tinu Elejogun 14 Marek Kostrzewski 25"
      + " Lily McGarrett 18 Olatunkbo Chijiaku 22 Adrienne Anthoula 22 Axelia Athanasios 22 Jon-Kabat Zinn 22 Thabang"
      + " Mosoa 15 Kgaogelo Mosoa 11 Multiplication table 1 2 3 1 1 2 3 2 2 4 6 3 3 6 9 In multi-dimensional "
      + "tables, each cell in the body of the table (and the value of that cell) relates to the values at the "
      + "beginnings of the column (i.e. the header), the row, and other structures in more complex tables. This is "
      + "an;injective relation each combination of the values of the headers row (row 0, for lack of a better term) "
      + "and the headers column (column 0 for lack of a better term) is related to a unique cell in the table";
  private static final String SHORT_DESCRIPTION =
      "Age table First name Last name Age Tinu Elejogun 14 Marek Kostrzewski25"
          + " Lily McGarrett 18 Olatunkbo Chijiaku 22 Adrienne Anthoula";

  private static final String BRAND = "BRAND";
  private static final String VALUE = "value1";
  private static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";
  private static final String DESCRIPTIVE_ATTRIBUTE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  public static final String UPC_CODE = "12345";
  public static final String LA_2000050 = "LA-2000050";
  public static final String LAIN_LAIN = "Lain-lain";
  public static final String BR_M036969 = "BR-M036969";
  public static final String DE_2000033 = "DE-2000033";
  public static final String RA_M000170 = "RA-M000170";
  public static final String OS_M000171 = "OS-M000171";
  public static final String DE_2000032 = "DE-2000032";
  public static final String JA_0036861 = "JA-0036861";
  public static final String SI_M000178 = "SI-M000178";
  public static final String DI_2000029 = "DI-2000029";
  public static final String RE_0036710 = "RE-0036710";
  public static final String ME_0036591 = "ME-0036591";
  public static final String ME_2000067 = "ME-2000067";
  public static final String KA_0000001 = "KA-0000001";
  public static final String RA_2000019 = "RA-2000019";
  public static final String IN_2000023 = "IN-2000023";
  public static final String BA_2000120 = "BA-2000120";
  public static final String KE_2000092 = "KE-2000092";
  public static final String KO_2000042 = "KO-2000042";
  public static final String LA_2000095 = "LA-2000095";
  public static final String FA_2000060 = "FA-2000060";
  public static final String WA_2000060 = "WA-2000060";
  public static final String TI_2000081 = "TI-2000081";
  public static final String AU_0036839 = "AU-0036839";
  public static final String BE_2000032 = "BE-2000032";
  public static final String DI_2000028 = "DI-2000028";
  public static final String DE_2000057 = "DE-2000057";
  public static final String VI_0036840 = "VI-0036840";
  public static final String LO_2000012 = "LO-2000012";
  public static final String TA_2000044 = "TA-2000044";
  public static final String TANGGAL_EVENT = "Tanggal Event";
  public static final String LOKASI_EVENT = "Lokasi Event";
  public static final String BRAND1 = "Brand";
  public static final String VIDEO = "Video";
  public static final String DETAIL_GARANSI = "Detail Garansi";
  public static final String DIMENSI_PRODUK = "Dimensi Produk";
  public static final String BERAT = "Berat";
  public static final String AUDIO = "Audio";
  public static final String TIPE_GARANSI = "Tipe Garansi";
  public static final String WARNA = "Warna";
  public static final String FAMILY_COLOUR = "Family Colour";
  public static final String LAMA_GARANSI = "Lama Garansi";
  public static final String KONDISI_PRODUK = "Kondisi Produk";
  public static final String KELENGKAPAN_PAKET = "Kelengkapan Paket";
  public static final String BATERAI = "Baterai";
  public static final String INTERFACE = "Interface";
  public static final String RAGAM_PESAN = "Ragam Pesan";
  public static final String KAMERA = "Kamera";
  public static final String MEMORY_EXTERNAL = "Memory External";
  public static final String MEMORI_INTERNAL = "Memori Internal";
  public static final String RESOLUSI = "Resolusi";
  public static final String DISPLAY = "Display";
  public static final String SIM_CARD = "Sim Card";
  public static final String JARINGAN = "Jaringan";
  public static final String DETAIL_OS = "Detail OS";
  public static final String OS = "OS";
  public static final String RAM = "RAM";
  public static final String DETAIL_PROSESSOR = "Detail Prosessor";
  public static final String VALID_VAL1 = "#!@1245";
  public static final String INVALID_VAL = "!@#$%";
  public static final String UPC_CODE1 = "12";
  public static final String UPC_CODE3 = "12245678";
  public static final String UPC_CODE4 = "1224567891012";
  public static final String UPC_CODE5 = "122456789101";
  public static final String UPC_CODE2 = "ABCDE";
  public static final String INVALID = "INVALID";
  public static final String HYPHEN = "-";
  public static final String POEM_TEST = "Poem Test";
  public static final String OEM = "OEM";
  public static final String ALLEN_SOLLY = "Allen Solly";
  public static final String ALLEN_SOLLY_TEST = "Allen Solly Test";
  public static final String ALLEN_SOLLY_WITHOUT_SPACE = "AllenSolly Test";
  public static final String EN_JI_BY_PALOMINO_YURIKO_HANDBAG_BLACK = "En-ji By Palomino Yuriko Handbag - Black";
  public static final String EN_JI_BY_PALOMINO = "EN-JI by Palomino";
  public static final String BRAND_2_WITHOUT_SPACE = "En-jiByPalomino Yuriko Handbag - Black";
  public static final String BRAND_2_WITHOUT_HYPHEN = "En ji By Palomino Yuriko Handbag - Black";

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private CategoryService categoryService;

  private ObjectMapper objectMapper = new ObjectMapper();
  private ClassLoader classLoader;
  private String filePath;

  @InjectMocks
  private ProductScoreUtilBean productScoreUtilBean;

  private ProductScoreRequest productScoreRequest;
  private CategoryDetailResponse categoryDetailResponse;
  private Map<String, MaxScoreAndRuleConfigResponse> productScoreRules;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    classLoader = getClass().getClassLoader();
    categoryDetailResponse = new CategoryDetailResponse();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productCategoryBaseOutbound, categoryService);
  }

  @Test
  public void getProductScoreByProductScoreRequest100Score() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    ProductScoreVo productScoreVo =
        productScoreUtilBean.getProductScoreByProductScoreRequest(generateProductScoreRequest(false, false));
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100.0,  0);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 15.0, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestCategoryDetailResponseNull() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () ->
              productScoreUtilBean.getProductScoreByProductScoreRequest(generateProductScoreRequest(false, false)));
    }
    finally {
      Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
      Mockito.verify(productCategoryBaseOutbound)
          .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
              CATEGORY_CODE);
    }
  }

  @Test
  public void getProductScoreByProductScoreRequest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    ProductScoreVo productScoreVo =
        productScoreUtilBean.getProductScoreByProductScoreRequest(generateProductScoreRequest(false, false));
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100.0,  0);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 15.0, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroNameMandatoryScore() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(null);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 11.25, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestSpecialCharsNameMandatoryScore() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(INVALID_VAL);
    productScoreRequest.setDescription(INVALID_VAL.getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 7.5, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroMandatoryScoreDescriptionNull() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription(null);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 11.25, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroMandatoryScoreDescriptionSpecialChars() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription("<p>- - - - - - - - - - - - - - - - - - - - - - - -</p>".getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getMandatoryAttributeScore(), 11.25, 0);
    Assertions.assertEquals( productScoreVo.getDescriptionScore(), 0, 0);
  }


  @Test
  public void getProductScoreByProductScoreRequestZeroProductTitle() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(NAME);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals( productScoreVo.getTotalScore(), 90,0);
    Assertions.assertEquals( productScoreVo.getProductTitleScore(), 0,0);
  }

  @Test
  public void getProductScoreByProductScoreRequestNullBrand() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(NAME);
    productScoreRequest.setBrand(null);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals( productScoreVo.getProductTitleScore(), 0,0);
  }

  @Test
  public void getProductScoreByProductScoreRequestNoIgnoreSymbolsProductTitleScore() throws Exception {
    //Basic case without ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(NAME + BRAND);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100,0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10,0);
  }

  @Test
  public void getProductScoreByProductScoreRequestWithIgnoreSymbolsProductTitleScore() throws Exception {
    //Basic case with ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(NAME + BRAND);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100,0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }


  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase1() throws Exception {
    // Single word brand , but product name doesn't have exact match of brand name. (Ignore symbols are empty)
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(POEM_TEST);
    productScoreRequest.setBrand(OEM);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals( productScoreVo.getProductTitleScore(),10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase2() throws Exception {
    // Single word brand , but product name doesn't have exact match of brand name. (Ignore symbols are not empty)
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(POEM_TEST);
    productScoreRequest.setBrand(OEM);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase3() throws Exception {
    // More than one word in brand name
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(ALLEN_SOLLY_TEST);
    productScoreRequest.setBrand(ALLEN_SOLLY);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase4() throws Exception {
    // More than one word in brand name , productname has brand without space , ignore symbols is not empty
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(ALLEN_SOLLY_WITHOUT_SPACE);
    productScoreRequest.setBrand(ALLEN_SOLLY);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase5() throws Exception {
    // More than one word in brand name , productname has brand without space , ignore symbols is empty
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(ALLEN_SOLLY_WITHOUT_SPACE);
    productScoreRequest.setBrand(ALLEN_SOLLY);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(90, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getProductTitleScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase6() throws Exception {
    // More than one word in brand name , more than one ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(Arrays.asList(StringUtils.SPACE, HYPHEN));
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(EN_JI_BY_PALOMINO_YURIKO_HANDBAG_BLACK);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase7() throws Exception {
    // More than one word in brand name , productname has brand without space , more than one ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(Arrays.asList(StringUtils.SPACE, HYPHEN));
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(BRAND_2_WITHOUT_SPACE);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase8() throws Exception {
    // More than one word in brand name , productname has brand without hyphen , more than one ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(Arrays.asList(StringUtils.SPACE, HYPHEN));
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(BRAND_2_WITHOUT_HYPHEN);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase9() throws Exception {
    // More than one word in brand name , productname has brand without hyphen , no ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(BRAND_2_WITHOUT_HYPHEN);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(90, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getProductTitleScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase10() throws Exception {
    // More than one word in brand name , productname same as brand , no ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(EN_JI_BY_PALOMINO_YURIKO_HANDBAG_BLACK);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getProductTitleScore(), 10, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestProductTitleScoreCase11() throws Exception {
    // More than one word in brand name , productname has brand without space , no ignore symbols
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    productScoreRuleResponse.setIgnoreSymbols(new ArrayList<>());
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setName(BRAND_2_WITHOUT_SPACE);
    productScoreRequest.setBrand(EN_JI_BY_PALOMINO);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(90, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getProductTitleScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroURLVideo() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setUrl(null);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(98, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getVideoUrlScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroDescription() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription(USP.getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(85, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestAllSpecialCharDescription() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription(INVALID_VAL.getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestEmptyDescription() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription(StringUtils.EMPTY.getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(81.25, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroUSP() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setUniqueSellingPoint(USP);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(95, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getUspScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestSpecialCharUSP() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setUniqueSellingPoint(INVALID_VAL);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(95, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getUspScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestHalfUSP() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setUniqueSellingPoint(SHORT_DESCRIPTION);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(97.5, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(2.5, productScoreVo.getUspScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestFullUSPFullDesc() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES1_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    ProductScoreVo productScoreVo =
        productScoreUtilBean.getProductScoreByProductScoreRequest(generateProductScoreRequest(false, false));
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100.0,  0);
    Assertions.assertEquals(5, productScoreVo.getUspScore(), 0);
  }

  @Test
  public void getRecommendedAttributeScoreNoWarrantySpecialAttributeTestNegativeTest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES1_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> attributeScoreRequest.getName()
            .equalsIgnoreCase(productScoreRuleResponse.getIgnoreAttributes().get(0).getAttributeName())).forEach(
        attributeScoreRequest -> attributeScoreRequest
            .setValues(Arrays.asList(productScoreRuleResponse.getIgnoreAttributes().get(0).getValue())));
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> productScoreRuleResponse.getIgnoreAttributes().get(0).getIgnoreAttributeNames()
            .contains(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(Arrays.asList(HYPHEN)));
    double productScoreVo = productScoreUtilBean
        .getRecommendedAttributeScore(productScoreRequest.getProductAttributeRequests(),
            productScoreRequest.getCategoryCode());
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo, 20, 0);
  }

  @Test
  public void getRecommendedAttributeScoreNoWarrantySpecialAttributeTestNegativeTest1() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRuleResponse.getIgnoreAttributes().get(0).setIgnoreAttributeNames(Arrays.asList(INVALID, DETAIL_GARANSI));
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> attributeScoreRequest.getName()
            .equalsIgnoreCase(productScoreRuleResponse.getIgnoreAttributes().get(0).getAttributeName())).forEach(
        attributeScoreRequest -> attributeScoreRequest
            .setValues(Arrays.asList(productScoreRuleResponse.getIgnoreAttributes().get(0).getValue())));
    productScoreRequest.getProductAttributeRequests().stream()
        .filter(attributeScoreRequest -> LAMA_GARANSI.equalsIgnoreCase(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(new ArrayList<>()));
    productScoreRequest.getProductAttributeRequests().stream()
        .filter(attributeScoreRequest -> DETAIL_GARANSI.equalsIgnoreCase(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(new ArrayList<>()));
    productScoreRequest.getProductAttributeRequests().stream()
        .filter(attributeScoreRequest -> LAIN_LAIN.equalsIgnoreCase(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(new ArrayList<>()));
    ProductScoreVo productScoreVo = productScoreUtilBean
        .getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(10, productScoreVo.getRecommendedAttributeScore(), 0);
  }

  @Test
  public void getRecommendedAttributeScoreDefiningEmptyValues() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    categoryDetailResponse.getCategoryAttributes().get(0).setMarkForDelete(true);
    productScoreRuleResponse.getIgnoreAttributes().get(0).setIgnoreAttributeNames(Arrays.asList(INVALID));
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> attributeScoreRequest.getName()
            .equalsIgnoreCase(productScoreRuleResponse.getIgnoreAttributes().get(0).getAttributeName())).forEach(
        attributeScoreRequest -> attributeScoreRequest
            .setValues(Arrays.asList(productScoreRuleResponse.getIgnoreAttributes().get(0).getValue())));
    productScoreRequest.getProductAttributeRequests().stream()
        .filter(attributeScoreRequest -> WARNA.equalsIgnoreCase(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(new ArrayList<>()));
    ProductScoreVo productScoreVo = productScoreUtilBean
        .getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo.getRecommendedAttributeScore(), 20, 0);
    Assertions.assertEquals(0, productScoreVo.getVariantCreatingScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestHalfDescription() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setDescription(categoryDetailResponse.getDescription());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(92.5, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(7.5, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestZeroImageScore() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests()
        .forEach(itemScoreRequest -> itemScoreRequest.setItemImages(new ArrayList<>()));
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(82.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOn() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.setSynchronised(true);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(87.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(5, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnMoreThan3() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.setSynchronised(true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true)));
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(92.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(10, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnOneNonCommonImage() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.setSynchronised(true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true)));
    productScoreRequest.getItemRequests().get(1).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, INTERFACE, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().get(2).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, INTERFACE, PRODUCT_CODE, 0, false)));
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(96.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(14, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnMoreThanOneNonCommonImage() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().get(1).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, INTERFACE, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().get(2).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, INTERFACE, PRODUCT_CODE, 0, false)));
    productScoreRequest.setSynchronised(true);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(100.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(18, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnMoreThanOneNonCommonImageSingleVariant() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().remove(1);
    productScoreRequest.getItemRequests().remove(1);
    productScoreRequest.setSynchronised(true);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(96.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(14, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnMoreThanOneNonCommonImageSingleVariant1() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().remove(1);
    productScoreRequest.getItemRequests().remove(1);
    productScoreRequest.setSynchronised(true);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(96.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(14, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestImageScoreWithCISwitchOnMoreThan3NonCommonImageSingleVariant() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, true);
    productScoreRequest.setSynchronised(true);
    productScoreRequest.getItemRequests().get(0).setItemImages(
        Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, true),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false),
            new MasterDataProductImageDTO(true, IN_2000023, PRODUCT_CODE, 0, false)));
    productScoreRequest.getItemRequests().remove(1);
    productScoreRequest.getItemRequests().remove(1);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(100.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(18, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestHalfImageScore() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setItemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0)));
    productScoreRequest.getItemRequests().get(1)
        .setItemImages(Arrays.asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0)));
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(91.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9, productScoreVo.getImageScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestTwoSkuTrueAttributes() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setProductAttributeRequests(productScoreRequest.getProductAttributeRequests().stream()
        .filter(attributeScoreRequest -> !attributeScoreRequest.getName().equalsIgnoreCase(LAMA_GARANSI))
        .collect(Collectors.toList()));
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(93.33, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(13.33, productScoreVo.getRecommendedAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestInvalidEANUPC() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE1);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(99.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.00, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestBlankEANUPC() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(null);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(99.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.00, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestValidEANUPC() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE3);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(100.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.0, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestValidEANUPC1() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE4);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(100.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.0, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestValidEANUPC12Digits() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE5);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(100.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.0, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestNonNumberEANUPC() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE2);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(99.00, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(9.00, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestEmptyOrNullOrSpecialAttribute() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(true, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode(UPC_CODE1);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(84.38, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(6.67, productScoreVo.getRecommendedAttributeScore(), 0);
    Assertions.assertEquals(7.71, productScoreVo.getRemainingAttributeScore(), 0);
  }

  @Test
  public void getRecommendedAttributeScoreEmptyOrNullOrSpecialAttribute() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(true, false);
    productScoreRequest.getItemRequests().get(0).setUpcCode("12");
    double productScoreVo = productScoreUtilBean
        .getRecommendedAttributeScore(productScoreRequest.getProductAttributeRequests(),
            productScoreRequest.getCategoryCode());
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo, 6.67, 0);
  }

  @Test
  public void getRecommendedAttributeScoreNoWarrantySpecialAttributeTest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> attributeScoreRequest.getName()
            .equalsIgnoreCase(productScoreRuleResponse.getIgnoreAttributes().get(0).getAttributeName())).forEach(
        attributeScoreRequest -> attributeScoreRequest
            .setValues(Arrays.asList(productScoreRuleResponse.getIgnoreAttributes().get(0).getValue())));
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> productScoreRuleResponse.getIgnoreAttributes().get(0).getIgnoreAttributeNames()
            .contains(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(Arrays.asList(HYPHEN)));
    double productScoreVo = productScoreUtilBean
        .getRecommendedAttributeScore(productScoreRequest.getProductAttributeRequests(),
            productScoreRequest.getCategoryCode());
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo, 20, 0);
  }

  @Test
  public void getRecommendedAttributeScoreValidWarrantySpecialAttributeTest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> productScoreRuleResponse.getIgnoreAttributes().get(0).getIgnoreAttributeNames()
            .contains(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(Arrays.asList(HYPHEN)));
    double productScoreVo = productScoreUtilBean
        .getRecommendedAttributeScore(productScoreRequest.getProductAttributeRequests(),
            productScoreRequest.getCategoryCode());
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo, 6.67, 0);
  }

  @Test
  public void getRecommendedAttributeScoreValidWarrantySpecialAttributeTestWithNullValue() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> productScoreRuleResponse.getIgnoreAttributes().get(0).getIgnoreAttributeNames()
            .contains(attributeScoreRequest.getName().toLowerCase()))
        .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(Arrays.asList(new String())));
    double productScoreVo = productScoreUtilBean
        .getRecommendedAttributeScore(productScoreRequest.getProductAttributeRequests(),
            productScoreRequest.getCategoryCode());
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(productScoreVo, 6.67, 0);
  }

  @Test
  public void
  getProductScoreByProductScoreRequestNoRecommendedAttributesAndZeroVariantCreationAttributes() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getRecommendedAttributeScore(), 20, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestNoRemainingAttributes() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_REMAINING_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRemainingAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getRecommendedAttributeScore(), 20, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestAndTestPopulateFunction() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_REMAINING_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRemainingAttributes();
    productScoreRequest.getProductAttributeRequests().get(1).setBasicView(false);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(productScoreVo.getTotalScore(), 100, 0);
    Assertions.assertEquals(productScoreVo.getRecommendedAttributeScore(), 20, 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestNoRemainingAttributesWrongAttributes() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_REMAINING_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRemainingAttributes();
    productScoreRequest.getProductAttributeRequests().get(2).setAttributeCode(INVALID);
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(93.33, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(13.33, productScoreVo.getRecommendedAttributeScore(), 0);
  }

  @Test
  public void testEvalFunctionGreaterThan() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.GREATER_THAN, 10, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(5, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionLessThan() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.LESS_THAN, 1000, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(5, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionLessThanNegativeCase() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.LESS_THAN, 1, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionLessThanEqualTo() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.LESS_THAN_OR_EQUAL_TO, 1000, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(5, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionLessThanEqualNegativeCaseTo() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.LESS_THAN_OR_EQUAL_TO, 1, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionNotEqualTo() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.NOT_EQUALS_TO, 1000, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(5, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionNotEqualToNegativeCase() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.NOT_EQUALS_TO, 1, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    productScoreRequest.setDescription(ProductScoreRuleConstants.DESCRIPTION_RULE.getBytes());
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }

  @Test
  public void testEvalFunctionDefault() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + NO_RECCOMENDED_ATTRIBUTES);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRules = new HashMap<>();
    MaxScoreAndRuleConfigResponse maxScoreAndRuleConfigResponse = new MaxScoreAndRuleConfigResponse();
    maxScoreAndRuleConfigResponse.setMaxScore(10);
    maxScoreAndRuleConfigResponse
        .setRuleConfig(Arrays.asList(new RuleConfigResponse(ProductScoreRuleConstants.BRAND, 1000, 5)));
    productScoreRules.put(ProductScoreRuleConstants.DESCRIPTION_RULE, maxScoreAndRuleConfigResponse);
    ProductScoreRuleResponse productScoreRuleResponse = new ProductScoreRuleResponse();
    productScoreRuleResponse.setProductScoreRules(productScoreRules);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE_1)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1)).thenReturn(categoryDetailResponse);
    productScoreRequest = generateProductScoreRequestNoRecommendedAttributes();
    ProductScoreVo productScoreVo = productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE_1);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE_1);
    Assertions.assertEquals(0, productScoreVo.getDescriptionScore(), 0);
  }


  private ProductScoreRequest generateProductScoreRequest(boolean isSpecialCharAttributeNeeded, boolean commonImage) throws IOException {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    productScoreRequest =
        ProductScoreRequest.builder().productCode(PRODUCT_CODE).brand(BRAND).categoryCode(CATEGORY_CODE)
            .name(NAME + StringUtils.SPACE + BRAND).url(USP).uniqueSellingPoint(DESCRIPTION)
            .description(categoryDetailResponse.getDefaultDescription()).productAttributeRequests(new ArrayList<>())
            .synchronised(false)
            .itemRequests(new ArrayList<>()).build();
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(BR_M036969).name(BRAND1).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(DE_2000033).name(DETAIL_PROSESSOR)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(RA_M000170).name(RAM).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(OS_M000171).name(OS).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(DE_2000032).name(DETAIL_OS).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(JA_0036861).name(JARINGAN).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(SI_M000178).name(SIM_CARD).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(DI_2000029).name(DISPLAY).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(RE_0036710).name(RESOLUSI).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(ME_0036591).name(MEMORI_INTERNAL)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(ME_2000067).name(MEMORY_EXTERNAL)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(KA_0000001).name(KAMERA).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(RA_2000019).name(RAGAM_PESAN)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(IN_2000023).name(INTERFACE).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(BA_2000120).name(BATERAI).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(KE_2000092).name(KELENGKAPAN_PAKET)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(LA_2000050).name(LAIN_LAIN).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(KO_2000042).name(KONDISI_PRODUK)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(true)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(LA_2000095).name(LAMA_GARANSI)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(FA_2000060).name(FAMILY_COLOUR)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(true).values(Arrays.asList(VALUE)).attributeCode(WA_2000060).name(WARNA).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, commonImage))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, commonImage))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, WA_2000060, PRODUCT_CODE, 0, commonImage),
            new MasterDataProductImageDTO(true, VI_0036840, PRODUCT_CODE, 0, commonImage))).build());
    if (isSpecialCharAttributeNeeded) {
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(true)
              .variantCreation(false).values(Arrays.asList(Constants.HYPHEN)).attributeCode(TI_2000081)
              .name(TIPE_GARANSI).build());//Invalid
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(VALID_VAL1)).name(AUDIO).attributeCode(AU_0036839)
              .build());//valid
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(INVALID_VAL)).name(BERAT).attributeCode(BE_2000032)
              .build());//Invalid
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(StringUtils.EMPTY)).name(DIMENSI_PRODUK)
              .attributeCode(DI_2000028).build());//Invalid
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(true)
              .variantCreation(false).values(Arrays.asList(StringUtils.SPACE)).name(DETAIL_GARANSI)
              .attributeCode(DE_2000057).build());//Invalid
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(new String())).name(VIDEO).attributeCode(VI_0036840)
              .build());//Invalid
    } else {
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(true)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(TI_2000081).name(TIPE_GARANSI)
              .build());
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(AU_0036839).name(AUDIO).build());
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(BE_2000032).name(BERAT).build());
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(DI_2000028).name(DIMENSI_PRODUK)
              .build());
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(true)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(DE_2000057).name(DETAIL_GARANSI)
              .build());
      productScoreRequest.getProductAttributeRequests().add(
          AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
              .variantCreation(false).values(Arrays.asList(VALUE)).attributeCode(VI_0036840).name(VIDEO).build());
    }
    return productScoreRequest;
  }

  private ProductScoreRequest generateProductScoreRequestNoRecommendedAttributes() {
    byte[] bytes = DESCRIPTION.getBytes();
    productScoreRequest =
        ProductScoreRequest.builder().productCode(PRODUCT_CODE).brand(BRAND).categoryCode(CATEGORY_CODE_1)
            .name(NAME + StringUtils.SPACE + BRAND).url(USP).uniqueSellingPoint(DESCRIPTION).description(bytes)
            .productAttributeRequests(new ArrayList<>()).itemRequests(new ArrayList<>()).build();
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).attributeCode(BR_M036969).values(Arrays.asList(VALUE)).name(BRAND1).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).attributeCode(LO_2000012).values(Arrays.asList(VALUE)).name(LOKASI_EVENT)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).attributeCode(TA_2000044).values(Arrays.asList(VALUE)).name(TANGGAL_EVENT)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(false).skuValue(false)
            .variantCreation(false).attributeCode(LA_2000050).values(Arrays.asList(VALUE)).name(LAIN_LAIN).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    return productScoreRequest;
  }

  private ProductScoreRequest generateProductScoreRequestNoRemainingAttributes() {
    byte[] bytes = DESCRIPTION.getBytes();
    productScoreRequest =
        ProductScoreRequest.builder().productCode(PRODUCT_CODE).brand(BRAND).categoryCode(CATEGORY_CODE_1)
            .name(NAME + StringUtils.SPACE + BRAND).url(USP).uniqueSellingPoint(DESCRIPTION).description(bytes)
            .productAttributeRequests(new ArrayList<>()).itemRequests(new ArrayList<>()).build();
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(PREDEFINED_ATTRIBUTE).basicView(true).skuValue(false)
            .variantCreation(false).attributeCode(BR_M036969).values(Arrays.asList(VALUE)).name(BRAND1).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DEFINING_ATTRIBUTE).basicView(true).skuValue(false)
            .variantCreation(true).attributeCode(WA_2000060).values(Arrays.asList(VALUE)).name(WARNA).build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(true).skuValue(false)
            .variantCreation(false).attributeCode(LO_2000012).values(Arrays.asList(VALUE)).name(LOKASI_EVENT)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(true).skuValue(false)
            .variantCreation(false).attributeCode(TA_2000044).values(Arrays.asList(VALUE)).name(TANGGAL_EVENT)
            .build());
    productScoreRequest.getProductAttributeRequests().add(
        AttributeScoreRequest.builder().attributeType(DESCRIPTIVE_ATTRIBUTE).basicView(true).skuValue(false)
            .variantCreation(false).attributeCode(LA_2000050).values(Arrays.asList(VALUE)).name(LAIN_LAIN).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    productScoreRequest.getItemRequests().add(ItemScoreRequest.builder().upcCode(UPC_CODE).itemImages(Arrays
        .asList(new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0),
            new MasterDataProductImageDTO(true, VALUE, PRODUCT_CODE, 0))).build());
    return productScoreRequest;
  }

  @Test
  public void getProductScoreByProductScoreRequest_emptyItemRequests() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    ProductScoreRequest productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setItemRequests(new ArrayList<>());
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    ProductScoreVo productScoreVo =
        productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
            CATEGORY_CODE);
    Assertions.assertEquals(81.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0.0, productScoreVo.getImageScore(), 0);
    Assertions.assertEquals(0.0, productScoreVo.getEanUpcScore(), 0);
  }

  @Test
  public void getProductScoreByProductScoreRequestEmptyUspRequestsTest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    ProductScoreRequest productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.setItemRequests(new ArrayList<>());
    productScoreRequest.setUniqueSellingPoint(null);
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse = objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse = objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound
      .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
        CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    ProductScoreVo productScoreVo =
      productScoreUtilBean.getProductScoreByProductScoreRequest(productScoreRequest);
    Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    Mockito.verify(productCategoryBaseOutbound)
      .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME,
        CATEGORY_CODE);
    Assertions.assertEquals(76.0, productScoreVo.getTotalScore(), 0);
    Assertions.assertEquals(0.0, productScoreVo.getImageScore(), 0);
    Assertions.assertEquals(0.0, productScoreVo.getEanUpcScore(), 0);
  }

  @Test
  public void getRecommendedAttributeScoreCategoryNullTest() throws Exception {
    filePath = classLoader.getResource(FILE_PATH).getPath();
    File file = new File(filePath + File.separator + IPHONE_JSON);
    CategoryDetailResponse categoryDetailResponse =
      objectMapper.readValue(file, CategoryDetailResponse.class);
    File rulesFile = new File(filePath + File.separator + RULES_JSON);
    ProductScoreRuleResponse productScoreRuleResponse =
      objectMapper.readValue(rulesFile, ProductScoreRuleResponse.class);
    Mockito.when(categoryService.getProductScoreRuleForCategory(CATEGORY_CODE))
      .thenReturn(productScoreRuleResponse);
    Mockito.when(productCategoryBaseOutbound.getCategoryDetailByCategoryCode(
        ProductScoreRuleConstants.REQUEST_ID, ProductScoreRuleConstants.USERNAME, CATEGORY_CODE))
      .thenReturn(null);
    productScoreRequest = generateProductScoreRequest(false, false);
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> attributeScoreRequest.getName()
          .equalsIgnoreCase(productScoreRuleResponse.getIgnoreAttributes().get(0).getAttributeName()))
      .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(
        Arrays.asList(productScoreRuleResponse.getIgnoreAttributes().get(0).getValue())));
    productScoreRequest.getProductAttributeRequests().stream().filter(
        attributeScoreRequest -> productScoreRuleResponse.getIgnoreAttributes().get(0)
          .getIgnoreAttributeNames().contains(attributeScoreRequest.getName().toLowerCase()))
      .forEach(attributeScoreRequest -> attributeScoreRequest.setValues(Arrays.asList(HYPHEN)));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> productScoreUtilBean.getRecommendedAttributeScore(
          productScoreRequest.getProductAttributeRequests(),
          productScoreRequest.getCategoryCode()));
    } finally {
      Mockito.verify(productCategoryBaseOutbound)
        .getCategoryDetailByCategoryCode(ProductScoreRuleConstants.REQUEST_ID,
          ProductScoreRuleConstants.USERNAME, CATEGORY_CODE);
      Mockito.verify(categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
    }
  }
}