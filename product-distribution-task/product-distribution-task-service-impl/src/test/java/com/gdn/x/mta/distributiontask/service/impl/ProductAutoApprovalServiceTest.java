package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gdn.x.mta.distributiontask.dao.api.ProductAutoApprovalRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.dto.AutoQcChangedFieldDto;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.enums.AutoQcConfigChangeStatus;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.response.AutoQcConfigChangeDto;
import com.gdn.x.mta.distributiontask.service.api.AutoQcConfigChangeService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.google.common.collect.ImmutableSet;

public class ProductAutoApprovalServiceTest {

  @InjectMocks
  private ProductAutoApprovalServiceImpl productAutoApprovalService;

  @Mock
  private ProductAutoApprovalRepository productAutoApprovalRepository;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private AutoQcConfigChangeService autoQcConfigChangeService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private ProductService productService;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<ProductAutoApproval> productAutoApprovalCaptor;

  @Captor
  private ArgumentCaptor<AutoQcConfigChange> autoQcConfigChangeArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<AutoQcConfigChange>> autoQcConfigChangeListArgumentCaptor;

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final Integer MAX_ALLOWED_PRODUCT_AUTO_APPROVALS = 100;
  private static final String MAX_RETY_COUNT = "5";
  private static final String BP_CODE_1 = "BP_CODE_1";
  private static final String BP_CODE_2 = "BP_CODE_2";
  private static final String CATEGORY_CODE_1 = "CATEGORY_CODE_1";
  private static final String CATEGORY_CODE_2 = "CATEGORY_CODE_2";
  private static final String MIN_PRODUCTS = "minProducts";
  private static final String OFFICIAL_STORE = "officialStore";

  private List<ProductAutoApproval> productAutoApprovals;
  private ProductAutoApproval productAutoApproval;
  private SystemParameterConfig maxRetryCountForAutoApprovalRetry;
  private List<AutoQcConfigChange> autoQcConfigChanges;
  private AutoQcConfigChangeDto autoQcConfigChangeDto;
  private AutoQcChangedFieldDto autoQcChangedFieldDto;
  private Map<String, AutoQcChangedFieldDto> autoQcChangedFieldDtoMap;
  private ObjectMapper mapper;
  private AutoQcConfigChange autoQcConfigChange;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    productAutoApprovals = new ArrayList<>();
    productAutoApproval = new ProductAutoApproval();
    productAutoApproval.setProductCode("MTA-100");
    productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.FAILED);
    productAutoApproval.setRetryCount(0);
    productAutoApprovals.add(productAutoApproval);
    maxRetryCountForAutoApprovalRetry = new SystemParameterConfig();
    maxRetryCountForAutoApprovalRetry.setValue(MAX_RETY_COUNT);

    AutoQcConfigChange autoQcConfigChange1 =
        new AutoQcConfigChange(BP_CODE_1, CATEGORY_CODE_1, "{}", true, AutoQcConfigChangeStatus.PENDING.name());
    AutoQcConfigChange autoQcConfigChange2 =
        new AutoQcConfigChange(BP_CODE_2, CATEGORY_CODE_2, "{}", false, AutoQcConfigChangeStatus.PENDING.name());
    autoQcConfigChange = new AutoQcConfigChange(BP_CODE_1, CATEGORY_CODE_1, StringUtils.SPACE, true,
        AutoQcConfigChangeStatus.PENDING.name());
    autoQcConfigChanges = Arrays.asList(autoQcConfigChange1, autoQcConfigChange2);
    autoQcConfigChangeDto = new AutoQcConfigChangeDto();
    autoQcConfigChangeDto.setSellerCode(BP_CODE_1);
    autoQcConfigChangeDto.setCategoryCode(CATEGORY_CODE_1);
    autoQcConfigChangeDto.setNewData(true);
    autoQcChangedFieldDto = new AutoQcChangedFieldDto();
    autoQcChangedFieldDto.setNewValue(String.valueOf("10"));
    autoQcChangedFieldDto.setOldValue(String.valueOf("1"));
    autoQcChangedFieldDtoMap = new HashMap<>();
    autoQcChangedFieldDtoMap.put(MIN_PRODUCTS, autoQcChangedFieldDto);
    autoQcConfigChangeDto.setChangeFieldResponseMap(autoQcChangedFieldDtoMap);

    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAutoApprovalRepository);
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
   void addProductsToAutoApprovalTable() throws Exception {
    ProductAutoApproval productAutoApproval1 = new ProductAutoApproval();
    productAutoApproval1.setCategoryCode(CATEGORY_CODE_1);
    Map<String, String> productCodeCategoryMap = new HashMap<>();
    productCodeCategoryMap.put(productAutoApproval.getProductCode(), CATEGORY_CODE_1);
    productCodeCategoryMap.put(productAutoApproval1.getProductCode(), CATEGORY_CODE_2);
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(productAutoApprovals);
    productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
        Arrays.asList(productAutoApproval1.getProductCode(),
          productAutoApproval.getProductCode()),productCodeCategoryMap);
    Mockito.verify(productAutoApprovalRepository).saveAll(Mockito.anyList());
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void findProductsToAutoApprovalOrderByCreatedDateAscTest(){
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndAutoApprovalStatusOrderByCreatedDateAsc
        (eq(STORE_ID), eq(AutoApprovalStatus.PENDING), any())).thenReturn(productAutoApprovals);
    List<ProductAutoApproval> productAutoApprovalList = productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateAsc(STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndAutoApprovalStatusOrderByCreatedDateAsc(eq(STORE_ID),
        eq(AutoApprovalStatus.PENDING), any());
    Assertions.assertEquals(productAutoApprovalList, productAutoApprovals);
  }

  @Test
   void findProductsToAutoApprovalOrderByUpdatedDateDescTest(){
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateDesc
        (eq(STORE_ID), eq(AutoApprovalStatus.PENDING), any())).thenReturn(productAutoApprovals);
    List<ProductAutoApproval> productAutoApprovalList = productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateDesc(STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateDesc(eq(STORE_ID),
            eq(AutoApprovalStatus.PENDING), any());
    Assertions.assertEquals(productAutoApprovalList, productAutoApprovals);
  }

  @Test
   void findProductsToAutoApprovalOrderByUpdatedDateAscTest(){
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateAsc
        (eq(STORE_ID), eq(AutoApprovalStatus.PENDING), any())).thenReturn(productAutoApprovals);
    List<ProductAutoApproval> productAutoApprovalList = productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateAsc(STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateAsc(eq(STORE_ID),
            eq(AutoApprovalStatus.PENDING), any());
    Assertions.assertEquals(productAutoApprovalList, productAutoApprovals);
  }

  @Test
   void findProductsToAutoApprovalOrderByCreatedDateDescTest(){
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndAutoApprovalStatusOrderByCreatedDateDesc
        (eq(STORE_ID), eq(AutoApprovalStatus.PENDING), any())).thenReturn(productAutoApprovals);
    List<ProductAutoApproval> productAutoApprovalList = productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateDesc(STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndAutoApprovalStatusOrderByCreatedDateDesc(eq(STORE_ID),
            eq(AutoApprovalStatus.PENDING), any());
    Assertions.assertEquals(productAutoApprovalList, productAutoApprovals);
  }

  @Test
   void updateProductAutoApprovalDetailsNATest(){
    productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.NA);
    productAutoApprovalService.updateProductAutoApprovalDetails(productAutoApproval);
    Mockito.verify(productAutoApprovalRepository).save(productAutoApprovalCaptor.capture());
    Assertions.assertTrue(productAutoApprovalCaptor.getValue().isMarkForDelete());
  }

  @Test
   void updateProductAutoApprovalDetailsFailedTest(){
    productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.FAILED);
    productAutoApprovalService.updateProductAutoApprovalDetails(productAutoApproval);
    Mockito.verify(productAutoApprovalRepository).save(productAutoApprovalCaptor.capture());
    Assertions.assertTrue(productAutoApprovalCaptor.getValue().isMarkForDelete());
  }

  @Test
   void updateProductAutoApprovalDetailsPublishedTest(){
    productAutoApproval.setAutoApprovalStatus(AutoApprovalStatus.PUBLISHED);
    productAutoApprovalService.updateProductAutoApprovalDetails(productAutoApproval);
    Mockito.verify(productAutoApprovalRepository).save(productAutoApprovalCaptor.capture());
    Assertions.assertFalse(productAutoApprovalCaptor.getValue().isMarkForDelete());
  }

  @Test
   void updateProductAutoApprovalDetailsByProductCodeTest(){
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCode(
        STORE_ID, PRODUCT_CODE)).thenReturn(productAutoApproval);
    productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(STORE_ID,
        PRODUCT_CODE, AutoApprovalStatus.FAILED, true);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAutoApprovalRepository).save(productAutoApproval);
  }

  @Test
   void updateProductAutoApprovalDetailsByProductCodeAndCategoryChangeFalseTest(){
    productAutoApproval.setCategoryCode(CATEGORY_CODE_1);
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCode(
        STORE_ID, PRODUCT_CODE)).thenReturn(productAutoApproval);
    productAutoApprovalService.updateProductAutoApprovalDetailsByProductCode(STORE_ID,
        PRODUCT_CODE, AutoApprovalStatus.FAILED, false);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productAutoApprovalRepository).save(productAutoApproval);
  }

  @Test
   void addProductsToAutoApprovalTableIncrementRetryCountTest() throws Exception {
    ProductAutoApproval productAutoApproval1 = new ProductAutoApproval();
    productAutoApproval1.setAutoApprovalStatus(AutoApprovalStatus.PUBLISHED);
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(List.of(productAutoApproval1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        Constants.MAX_RETRY_COUNT_FOR_AUTO_APPROVAL_RETRY)).thenReturn(maxRetryCountForAutoApprovalRetry);
    productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(productAutoApproval1.getProductCode()), Collections.emptyMap());
    Mockito.verify(productAutoApprovalRepository).saveAll(Mockito.anyList());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        Constants.MAX_RETRY_COUNT_FOR_AUTO_APPROVAL_RETRY);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void addProductsToAutoApprovalTableIncrementRetryCountFailedTest() throws Exception {
    ProductAutoApproval productAutoApproval1 = new ProductAutoApproval();
    productAutoApproval1.setAutoApprovalStatus(AutoApprovalStatus.PUBLISHED);
    productAutoApproval1.setRetryCount(Integer.parseInt(MAX_RETY_COUNT)+1);
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(List.of(productAutoApproval1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        Constants.MAX_RETRY_COUNT_FOR_AUTO_APPROVAL_RETRY)).thenReturn(maxRetryCountForAutoApprovalRetry);
    productAutoApprovalService.addProductsToAutoApprovalTable(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(productAutoApproval1.getProductCode()), Collections.emptyMap());
    Mockito.verify(productAutoApprovalRepository).saveAll(Mockito.anyList());
    Mockito.verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        Constants.MAX_RETRY_COUNT_FOR_AUTO_APPROVAL_RETRY);
    Mockito.verify(productAutoApprovalRepository)
        .findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void processPendingAutoQcConfigChangeTest() throws IOException {
    Pageable pageable1 = PageRequest.of(0, 1, Sort.by(new Sort.Order(Sort.Direction.ASC, Constants.CREATED_DATE)));
    Pageable pageable2 = PageRequest.of(1, 1, Sort.by(new Sort.Order(Sort.Direction.ASC, Constants.CREATED_DATE)));
    Page<Product> productPage = new PageImpl<>(List.of(new Product()), pageable1, 2);
    Mockito.when(productAutoApprovalRepository.findByStoreIdAndProductCodeIn(Mockito.anyString(),
        Mockito.anyList())).thenReturn(productAutoApprovals);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE, "2",
            Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constants.PRODUCT_AUTO_APPROVAL_INSERT_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(Constants.PRODUCT_AUTO_APPROVAL_INSERT_BATCH_SIZE, "1",
            Constants.PRODUCT_AUTO_APPROVAL_INSERT_BATCH_SIZE));
    Mockito.when(
        autoQcConfigChangeService.fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(),
            2)).thenReturn(autoQcConfigChanges);
    Mockito.when(autoQcConfigChangeService
        .saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture())).thenReturn(autoQcConfigChanges);
    Mockito.when(productServiceRepository.validateAutoQcConfigChange(
        ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(0)))).thenReturn(true);
    Mockito.when(productServiceRepository.validateAutoQcConfigChange(
        ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(1)))).thenReturn(false);
    Mockito.when(
            productServiceRepository.getCnCategoryCodesFromC1(Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(new CategoryCodeResponse(List.of(CATEGORY_CODE_2)));
    Mockito.when(
        productService.getProductsBySellerCodeAndCategoryCodes(STORE_ID, BP_CODE_1, WorkflowState.IN_REVIEW,
            ImmutableSet.of(CATEGORY_CODE_2), pageable1)).thenReturn(productPage);
    Mockito.when(
        productService.getProductsBySellerCodeAndCategoryCodes(STORE_ID, BP_CODE_1, WorkflowState.IN_REVIEW,
            ImmutableSet.of(CATEGORY_CODE_2), pageable2)).thenReturn(null);
    Mockito.when(productAutoApprovalRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(autoQcConfigChangeService.saveAutoQcConfigChange(autoQcConfigChangeArgumentCaptor.capture()))
        .thenReturn(new AutoQcConfigChange());

    productAutoApprovalService.processPendingAutoQcConfigChange(STORE_ID);

    Mockito.verify(autoQcConfigChangeService)
        .fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(), 2);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constants.PRODUCT_AUTO_APPROVAL_INSERT_BATCH_SIZE);
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.getValue());
    Mockito.verify(productServiceRepository)
        .validateAutoQcConfigChange(ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(0)));
    Mockito.verify(productServiceRepository)
        .validateAutoQcConfigChange(ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(1)));
    Mockito.verify(productServiceRepository)
        .getCnCategoryCodesFromC1(Mockito.any(CategoryCodeRequest.class));
    Mockito.verify(productService, Mockito.times(2))
        .getProductsBySellerCodeAndCategoryCodes(eq(STORE_ID), eq(BP_CODE_1), eq(WorkflowState.IN_REVIEW),
            Mockito.any(), Mockito.any(Pageable.class));
    Mockito.verify(productAutoApprovalRepository, Mockito.times(2)).saveAll(Mockito.anyList());
    Mockito.verify(autoQcConfigChangeService, Mockito.times(2))
        .saveAutoQcConfigChange(autoQcConfigChangeArgumentCaptor.capture());
    Mockito.verify(productAutoApprovalRepository, Mockito.times(2))
        .findByStoreIdAndProductCodeIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void processPendingAutoQcConfigEmptyResponseTest() throws IOException {
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE, "2",
            Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE));
    Mockito.when(
        autoQcConfigChangeService.fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(),
            2)).thenReturn(new ArrayList<>());

    productAutoApprovalService.processPendingAutoQcConfigChange(STORE_ID);

    Mockito.verify(autoQcConfigChangeService)
        .fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(), 2);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE);
  }

  @Test
   void processPendingAutoQcConfigChangeExceptionTest() throws IOException {
    Pageable pageable1 = PageRequest.of(0, 1, Sort.by(new Sort.Order(Sort.Direction.ASC, Constants.CREATED_DATE)));
    Page<Product> productPage = new PageImpl<>(List.of(new Product()), pageable1, 2);

    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID,
        Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE)).thenReturn(
        new SystemParameterConfig(Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE, "2",
            Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE));
    Mockito.when(
        autoQcConfigChangeService.fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(),
            2)).thenReturn(autoQcConfigChanges);
    Mockito.when(autoQcConfigChangeService
        .saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture())).thenReturn(autoQcConfigChanges);
    Mockito.when(productServiceRepository.validateAutoQcConfigChange(
        ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(0)))).thenThrow(RuntimeException.class);
    Mockito.when(autoQcConfigChangeService.saveAutoQcConfigChange(autoQcConfigChangeArgumentCaptor.capture()))
        .thenReturn(new AutoQcConfigChange());

    productAutoApprovalService.processPendingAutoQcConfigChange(STORE_ID);

    Mockito.verify(autoQcConfigChangeService)
        .fetchAutoQcConfigChangesByStatus(STORE_ID, AutoQcConfigChangeStatus.PENDING.name(), 2);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, Constants.AUTO_QC_CONFIG_CHANGE_FETCH_BATCH_SIZE);
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.getValue());
    Mockito.verify(productServiceRepository)
        .validateAutoQcConfigChange(ConverterUtil.toAutoQcConfigChangeRequest(autoQcConfigChanges.get(0)));
  }

  @Test
   void saveAutoQcConfigChangesTest() throws IOException {
    Mockito.when(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()))
        .thenReturn(mapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap());
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
    Assertions.assertEquals(mapper.writeValueAsString(autoQcChangedFieldDtoMap),
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getChangedFields());
  }

  @Test
  void saveAutoQcConfigChangesTest_nullChangeFieldResponse() throws IOException {
    autoQcConfigChangeDto.setChangeFieldResponseMap(null);
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
    Assertions.assertNull(
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getChangedFields());
  }

  @Test
   void saveAutoQcConfigChangesExistsAlreadyTest() throws IOException {
    Mockito.when(autoQcConfigChangeService
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name()))
        .thenReturn(autoQcConfigChange);
    Mockito.when(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()))
        .thenReturn(mapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    autoQcConfigChangeDto.setChangeFieldResponseMap(new HashMap<>());
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
  }

  @Test
   void saveAutoQcConfigChangesExistsAlreadyWithNewChangeTest() throws IOException {
    Mockito.when(autoQcConfigChangeService
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name()))
        .thenReturn(autoQcConfigChange);
    Mockito.when(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()))
        .thenReturn(mapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap());
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
    Assertions.assertEquals(mapper.writeValueAsString(autoQcChangedFieldDtoMap),
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getChangedFields());
  }

  @Test
   void saveAutoQcConfigChangesExistsAlreadyWithNewChange1Test() throws IOException {
    Map<String, AutoQcChangedFieldDto> autoQcChangedFieldDtoMap = new HashMap<>();
    Map<String, ChangedFieldDto> changedFieldDtoHashMap = new HashMap<>();
    changedFieldDtoHashMap.put(MIN_PRODUCTS, new ChangedFieldDto("1", "10"));
    changedFieldDtoHashMap.put(OFFICIAL_STORE, new ChangedFieldDto("false", "true"));
    autoQcChangedFieldDtoMap.put(OFFICIAL_STORE, new AutoQcChangedFieldDto("false", "true"));
    autoQcConfigChange.setChangedFields(mapper.writeValueAsString(autoQcChangedFieldDtoMap));
    Mockito.when(objectMapper.readValue(Mockito.any(String.class), Mockito.any(TypeReference.class))).thenReturn(mapper
        .readValue(mapper.writeValueAsString(autoQcChangedFieldDtoMap),
            new TypeReference<HashMap<String, ChangedFieldDto>>() {
            }));
    Mockito.when(autoQcConfigChangeService
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name()))
        .thenReturn(autoQcConfigChange);
    Mockito.when(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()))
        .thenReturn(mapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(changedFieldDtoHashMap);
    Mockito.verify(objectMapper)
        .readValue(Mockito.eq(mapper.writeValueAsString(autoQcChangedFieldDtoMap)), Mockito.any(TypeReference.class));
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
  }

  @Test
   void saveAutoQcConfigChangesExistsAlreadyWithNewChange2Test() throws IOException {
    Map<String, AutoQcChangedFieldDto> autoQcChangedFieldDtoMap = new HashMap<>();
    Map<String, ChangedFieldDto> changedFieldDtoHashMap = new HashMap<>();
    changedFieldDtoHashMap.put(MIN_PRODUCTS, new ChangedFieldDto("1", "10"));
    changedFieldDtoHashMap.put(OFFICIAL_STORE, new ChangedFieldDto("false", "true"));
    autoQcChangedFieldDtoMap.put(MIN_PRODUCTS, new AutoQcChangedFieldDto("1", "10"));
    autoQcConfigChange.setChangedFields(mapper.writeValueAsString(autoQcChangedFieldDtoMap));
    Mockito.when(objectMapper.readValue(Mockito.any(String.class), Mockito.any(TypeReference.class))).thenReturn(mapper
        .readValue(mapper.writeValueAsString(autoQcChangedFieldDtoMap),
            new TypeReference<HashMap<String, ChangedFieldDto>>() {
            }));
    Mockito.when(autoQcConfigChangeService
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name()))
        .thenReturn(autoQcConfigChange);
    Mockito.when(objectMapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()))
        .thenReturn(mapper.writeValueAsString(autoQcConfigChangeDto.getChangeFieldResponseMap()));
    productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    Mockito.verify(autoQcConfigChangeService)
        .findBySellerCodeAndC1CategoryCodeAndStatus(autoQcConfigChangeDto.getSellerCode(),
            autoQcConfigChangeDto.getCategoryCode(), AutoQcConfigChangeStatus.PENDING.name());
    Mockito.verify(autoQcConfigChangeService).saveAutoQcConfigChanges(autoQcConfigChangeListArgumentCaptor.capture());
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(objectMapper)
        .readValue(Mockito.eq(mapper.writeValueAsString(autoQcChangedFieldDtoMap)), Mockito.any(TypeReference.class));
    Assertions.assertEquals(BP_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getSellerCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        autoQcConfigChangeListArgumentCaptor.getValue().get(0).getC1CategoryCode());
    Assertions.assertTrue(autoQcConfigChangeListArgumentCaptor.getValue().get(0).isNewSeller());
  }

}
