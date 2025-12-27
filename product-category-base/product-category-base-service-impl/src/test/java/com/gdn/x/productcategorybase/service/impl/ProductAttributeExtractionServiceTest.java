package com.gdn.x.productcategorybase.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ExtractionStatus;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.outbound.matrix.MatrixOutbound;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import com.gdn.x.productcategorybase.repository.ProductAttributeExtractedRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

public class ProductAttributeExtractionServiceTest {
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE = "catgeoryCode";
  private static final String CATEGORY_CODE_2 = "catgeoryCode2";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String PRODUCT_NAME = "productName";
  private static final String UNIQUE_SELLING_POINT = "usp";
  private static final String DESCRIPTION = "description";
  private static final String BRAND_NAME = "brand-name";
  private static final String ATTRIBUTE_RESPONSE_JSON =
      "{" + "      \"Storage Type\": [\n" + "        \"SSD\"\n" + "      ],\n" + "      \"SSD/EMMC Storage\": {\n"
          + "        \"SSD\": \"256GB\"\n" + "      },\n" + "      \"HDD Storage\": \"\",\n"
          + "      \"Battery Backup\": \"\",\n" + "      \"Display\": \"\",\n" + "      \"Memory\": \"16GB\",\n"
          + "      \"RAM Type\": \"\",\n" + "      \"Prosessor brand\": \"Intel\",\n"
          + "      \"Resolusi Layar\": \"\",\n" + "      \"Touchscreen\": \"Tidak Ada\",\n"
          + "      \"Lama Garansi\": \"\",\n" + "      \"Tipe Garansi\": \"\",\n" + "      \"Weight\": \"\",\n"
          + "      \"Graphics Brand\": \"\",\n" + "      \"Graphics Memory\": \"\",\n"
          + "      \"Wireless\": \"Tidak Ada\",\n" + "      \"Keyboard\": \"Tidak Ada\",\n"
          + "      \"Mouse\": \"Tidak Ada\",\n" + "      \"Monitor\": \"Tidak Ada\",\n"
          + "      \"Processor name\": \"Core i9\",\n" + "      \"Processor generation\": \"11th generation\",\n"
          + "      \"Laptop Model no\": \"\"\n" + "    }";

  @InjectMocks
  private ProductAttributeExtractionServiceImpl productAttributeExtractionService;

  @Mock
  private ProductAttributeExtractedRepository productAttributeExtractedRepository;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ProductService productService;

  @Mock
  private MatrixOutbound matrixOutbound;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Captor
  private ArgumentCaptor<ProductAttributeExtracted> productAttributeExtractedArgumentCaptor;

  private ProductAttributeExtracted productAttributeExtracted;
  private Category category;
  private ProductAttributeExtractionModel productAttributeExtractionModel;
  private Product product;
  private MatrixAttributeExtractionRequest matrixAttributeExtractionRequest;
  private MatrixAttributeExtractionResponse matrixAttributeExtractionResponse;
  private HashMap<String, Object> attributeMap;
  private Pageable pageableDesc;
  private Pageable pageableAsc;

  @BeforeEach
  public void setUp() throws IOException {
    MockitoAnnotations.initMocks(this);

    productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(PRODUCT_CODE);
    productAttributeExtracted.setCnCategoryCode(CATEGORY_CODE);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);

    category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME);
    category.setExtractionType(ExtractionType.TEXT);

    productAttributeExtractionModel = new ProductAttributeExtractionModel();
    productAttributeExtractionModel.setCnCategoryCode(CATEGORY_NAME);
    productAttributeExtractionModel.setProductCode(PRODUCT_CODE);
    productAttributeExtractionModel.setExtractionType(ExtractionType.TEXT.name());
    productAttributeExtractionModel.setStoreId(STORE_ID);

    product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setName(PRODUCT_NAME);
    product.setBrand(BRAND_NAME);
    product.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    product.setDescription(DESCRIPTION.getBytes());

    matrixAttributeExtractionRequest = ConverterUtil.toMatrixAttributeExtractionRequest(product, productAttributeExtractionModel);

    attributeMap = new ObjectMapper().readValue(ATTRIBUTE_RESPONSE_JSON, HashMap.class);
    matrixAttributeExtractionResponse = new MatrixAttributeExtractionResponse();
    matrixAttributeExtractionResponse.setExtractedAttributes(attributeMap);

    pageableDesc = PageRequest.of(0, 1,
         Sort.by(Sort.Direction.DESC, Constants.UPDATED_DATE).and(Sort.by(Sort.Direction.ASC, Constants.ID)));

    pageableAsc = PageRequest.of(0, 1,
        Sort.by(Sort.Direction.ASC, Constants.UPDATED_DATE).and(Sort.by(Sort.Direction.ASC, Constants.ID)));

    Mockito.when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER)).thenReturn(
        new SystemParameter(Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER, "1",
            Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER));
    Mockito.when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE)).thenReturn(
        new SystemParameter(Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE, "1",
            Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE));
    Mockito.when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION)).thenReturn(
        new SystemParameter(Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION, "DESC",
            Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productAttributeExtractedRepository);
    Mockito.verifyNoMoreInteractions(categoryService);
    Mockito.verifyNoMoreInteractions(domainEventPublisherService);
    Mockito.verifyNoMoreInteractions(systemParameterService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void publishPendingProductsForBackfillingTest() {
    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc))
        .thenReturn(new PageImpl<>(Arrays.asList(productAttributeExtracted)));
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(category));
    Mockito.when(domainEventPublisherService
        .publishProductAttributeExtractionBackfillingEvent(STORE_ID, USERNAME, PRODUCT_CODE, CATEGORY_CODE,
            CATEGORY_NAME, ExtractionType.TEXT.name())).thenReturn(new ProductAttributeExtractionModel());
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture()))
        .thenReturn(productAttributeExtracted);

    productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc);
    Mockito.verify(categoryService).findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());

    Assertions.assertEquals(ExtractionStatus.IN_PROGRESS, productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(USERNAME, productAttributeExtractedArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  public void publishPendingProductsForBackfillingNullCategoryTest() {
    category.setCategoryCode(CATEGORY_CODE_2);

    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc))
        .thenReturn(new PageImpl<>(Arrays.asList(productAttributeExtracted)));
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(category));

    productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc);
    Mockito.verify(categoryService).findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void publishPendingProductsForBackfillingNullExtractionTest() {
    category.setExtractionType(null);

    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc))
        .thenReturn(new PageImpl<>(Arrays.asList(productAttributeExtracted)));
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(category));

    productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc);
    Mockito.verify(categoryService).findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void publishPendingProductsForBackfillingNoPendingRecordTest() {
    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(category));

    productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableDesc);
  }

  @Test
  public void publishPendingProductsForBackfillingErrorTest() {
    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING,
           pageableAsc)).thenReturn(new PageImpl<>(Arrays.asList(productAttributeExtracted)));
    Mockito.when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION)).thenReturn(
        new SystemParameter(Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION, "ASC",
            Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION));
    Mockito.when(categoryService.findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenThrow(ApplicationRuntimeException.class);

    productAttributeExtractionService.publishProductsForBackfillingByStatus(STORE_ID, USERNAME, ExtractionStatus.PENDING.name());

    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_NUMBER);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_MAX_PAGE_SIZE);
    Mockito.verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_ATTRIBUTE_EXTRACTION_SORT_ORDER_CONFIGURATION);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndStatusAndMarkForDeleteFalse(STORE_ID, ExtractionStatus.PENDING, pageableAsc);
    Mockito.verify(categoryService).findByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void updateExtractedAttributesForBackfillingTest() throws JsonProcessingException {
    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(matrixOutbound.extractProductAttributesByTextDetails(matrixAttributeExtractionRequest))
        .thenReturn(new SingleBaseResponse<>(true, null, null, matrixAttributeExtractionResponse));
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture())).thenReturn(productAttributeExtracted);
    Mockito.when(objectMapper.writeValueAsString(attributeMap)).thenReturn(ATTRIBUTE_RESPONSE_JSON);

    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(
        productAttributeExtractedRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(matrixOutbound).extractProductAttributesByTextDetails(matrixAttributeExtractionRequest);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(attributeMap);

    Assertions.assertEquals(ATTRIBUTE_RESPONSE_JSON, productAttributeExtractedArgumentCaptor.getValue().getExtractedAttribute());
    Assertions.assertEquals(ExtractionStatus.SUCCESS, productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());
  }

  @Test
  public void updateExtractedAttributesForBackfillingMatrixErrorTest() {
    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(matrixOutbound.extractProductAttributesByTextDetails(matrixAttributeExtractionRequest)).thenReturn(
        new SingleBaseResponse<>(false, ErrorMessage.SYSTEM_ERROR.name(), ErrorMessage.SYSTEM_ERROR.getMessage(),
            matrixAttributeExtractionResponse));
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture()))
        .thenReturn(productAttributeExtracted);

    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(matrixOutbound).extractProductAttributesByTextDetails(matrixAttributeExtractionRequest);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());

    Assertions.assertEquals(ExtractionStatus.FAILED, productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(ErrorMessage.SYSTEM_ERROR.name() + "-" + ErrorMessage.SYSTEM_ERROR.getMessage(),
        productAttributeExtractedArgumentCaptor.getValue().getErrorMessage());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());
  }

  @Test
  public void updateExtractedAttributesForBackfillingTextImageTest() {
    productAttributeExtractionModel.setExtractionType(ExtractionType.TEXT_IMAGE.name());

    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture())).thenReturn(productAttributeExtracted);

    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(
        productAttributeExtractedRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());

    Assertions.assertEquals(ExtractionStatus.PENDING, productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());
  }

  @Test
  public void updateExtractedAttributesForBackfillingExceptionTestTest() {
    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture()))
        .thenReturn(productAttributeExtracted);

    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());

    Assertions.assertEquals(ExtractionStatus.FAILED, productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(ErrorMessage.SYSTEM_ERROR.getMessage(),
        productAttributeExtractedArgumentCaptor.getValue().getErrorMessage());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());

  }

  @Test
  public void updateExtractedAttributesForBackfillingInlineTextTest() throws Exception {
    productAttributeExtractionModel.setExtractionType(ExtractionType.INLINE_TEXT.name());
    productAttributeExtracted.setStoreId(STORE_ID);
    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
      .thenReturn(product);
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture())).thenReturn(productAttributeExtracted);

    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(
      productAttributeExtractedRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());
    Mockito.verify(productServiceWrapper).autoFillAttributes(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(ExtractionStatus.SUCCESS,
      productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());
  }

  @Test
  public void updateExtractedAttributesForBackfillingInlineTextWithExceptionTest() throws Exception {
    productAttributeExtractionModel.setExtractionType(ExtractionType.INLINE_TEXT.name());
    productAttributeExtracted.setStoreId(STORE_ID);
    Mockito.when(
        productAttributeExtractedRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productAttributeExtracted);
    Mockito.when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE))
      .thenReturn(product);
    Mockito.when(productAttributeExtractedRepository.save(productAttributeExtractedArgumentCaptor.capture())).thenReturn(productAttributeExtracted);
    Mockito.doThrow(Exception.class).when(productServiceWrapper).autoFillAttributes(STORE_ID,
      PRODUCT_CODE);
    productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);

    Mockito.verify(
      productAttributeExtractedRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());
    Mockito.verify(productServiceWrapper).autoFillAttributes(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(ExtractionStatus.FAILED,
      productAttributeExtractedArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constants.MATRIX, productAttributeExtractedArgumentCaptor.getValue().getSource());
  }

  @Test
  public void addProductsToProductAttributeExtractionWithNullExtractionType() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    productAttributeExtractionService.addProductsToProductAttributeExtraction(PRODUCT_CODE, category);
  }

  @Test
  public void addProductsToProductAttributeExtractionWithNonNullExtractionType() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setExtractionType(ExtractionType.TEXT);
    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).
        thenReturn(null);
    productAttributeExtractionService.addProductsToProductAttributeExtraction(PRODUCT_CODE, category);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());
    Assertions.assertEquals(productAttributeExtractedArgumentCaptor.getValue().getStatus(), ExtractionStatus.PENDING);
  }

  @Test
  public void addProductsToProductAttributeExtractionWithNonNullExtractionTypeAlreadyExists() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setExtractionType(ExtractionType.TEXT);
    Mockito.when(productAttributeExtractedRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).
        thenReturn(new ProductAttributeExtracted());
    productAttributeExtractionService.addProductsToProductAttributeExtraction(PRODUCT_CODE, category);
    Mockito.verify(productAttributeExtractedRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAttributeExtractedRepository).save(productAttributeExtractedArgumentCaptor.capture());
    Assertions.assertEquals(productAttributeExtractedArgumentCaptor.getValue().getStatus(), ExtractionStatus.PENDING);
  }

  @Test
  public void fetchAndSetProductAttributeExtractionTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setExtractionType(ExtractionType.TEXT);
    Mockito.when(productAttributeExtractedRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).
      thenReturn(null);
    productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      category, STORE_ID);
    Mockito.verify(productAttributeExtractedRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void fetchAndSetProductAttributeExtraction_withNullExtractionType() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      category, STORE_ID);
  }

  @Test
  public void fetchAndSetProductAttributeExtractionNonNullExtractionTypeAlreadyExists() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setExtractionType(ExtractionType.TEXT);
    Mockito.when(productAttributeExtractedRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).
      thenReturn(new ProductAttributeExtracted());
    productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      category, STORE_ID);
    Mockito.verify(productAttributeExtractedRepository)
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void fetchAndSetProductAttributeExtractionInlineTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    Mockito.when(productAttributeExtractedRepository
      .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString())).
      thenReturn(null);
    productAttributeExtractionService.fetchAndSetProductAttributeExtraction(PRODUCT_CODE,
      category, STORE_ID);
  }

  @Test
  public void saveProductToAttributeExtractionTest() {
    ProductAttributeExtracted productAttributeExtracted;
    productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(PRODUCT_CODE);
    productAttributeExtracted.setCnCategoryCode(CATEGORY_CODE);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
    Mockito.when(this.productAttributeExtractedRepository.saveAndFlush(productAttributeExtracted)).thenReturn(productAttributeExtracted);
    productAttributeExtractionService.saveProductToAttributeExtraction(productAttributeExtracted);
    Mockito.verify(this.productAttributeExtractedRepository).saveAndFlush(productAttributeExtracted);

  }

  @Test
  public void saveProductToAttributeExtractionNullTest() {
    ProductAttributeExtracted productAttributeExtracted;
    productAttributeExtracted = new ProductAttributeExtracted();
    productAttributeExtracted.setProductCode(PRODUCT_CODE);
    productAttributeExtracted.setCnCategoryCode(CATEGORY_CODE);
    productAttributeExtracted.setStatus(ExtractionStatus.PENDING);
    Mockito.when(this.productAttributeExtractedRepository.saveAndFlush(productAttributeExtracted)).thenReturn(productAttributeExtracted);
    productAttributeExtractionService.saveProductToAttributeExtraction(null);

  }
}
