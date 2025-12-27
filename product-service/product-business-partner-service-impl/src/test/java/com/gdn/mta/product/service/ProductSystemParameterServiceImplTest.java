package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.ProductSystemParameterRepository;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;

public class ProductSystemParameterServiceImplTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DESCRIPTION = "desc";
  private static final String VARIABLE = "variable";
  private static final String VALUE = "value";
  private static final String VALUE_1 = "value1";
  private static final String VALUE_2 = "true";
  private static final String VALUE_3 = "false";
  private static final String ID = "id";
  private static final String VARIABLE_1 = "variable1";
  private static final String VARIABLE_2 = "variable2";
  private static final String VARIABLE_3 = "variable3";
  private static final String VARIABLE_4 = "variable4";
  private static final String SWITCH_VARIALBLES = "variable1,variable2";
  private static final String TEXT_VARIALBLES = "variable3,variable4";
  private List<String> SWITCH_VARIALBLES_LIST = new ArrayList<>();
  private List<String> TEXT_VARIABLE_LIST = new ArrayList<>();
  private ProductSystemParameter productSystemParameter;
  private ProductSystemParameter productSystemParameter1;
  private ProductSystemParameter productSystemParameter2;
  private ProductSystemParameter productSystemParameter3;


  @InjectMocks
  private ProductSystemParameterServiceImpl productSystemParameterService;

  @Mock
  private ProductSystemParameterRepository productSystemParameterRepository;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private PreOrderConfig preOrderConfig;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productSystemParameterService, "switchVariables", SWITCH_VARIALBLES);
    ReflectionTestUtils.setField(productSystemParameterService, "textVariables", TEXT_VARIALBLES);
    ReflectionTestUtils.setField(productSystemParameterService, "maxStockValue", 10);
    productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setDescription(DESCRIPTION);
    productSystemParameter.setValue(VALUE);
    productSystemParameter.setVariable(VARIABLE_1);
    productSystemParameter.setStoreId(DEFAULT_STORE_ID);
    productSystemParameter.setId(ID);

    productSystemParameter1 = new ProductSystemParameter();
    BeanUtils.copyProperties(productSystemParameter, productSystemParameter1);
    productSystemParameter1.setValue(VALUE_1);

    SWITCH_VARIALBLES_LIST.add(VARIABLE_1);
    SWITCH_VARIALBLES_LIST.add(VARIABLE_2);

    TEXT_VARIABLE_LIST = new ArrayList<>(Arrays.asList(VARIABLE_1, VARIABLE_2, VARIABLE_3, VARIABLE_4));

    productSystemParameter2 = new ProductSystemParameter(SystemParameterConstants.MAX_STOCK_LIMIT, "10",
        SystemParameterConstants.MAX_STOCK_LIMIT, false);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productSystemParameterRepository, applicationContext, preOrderConfig);
  }

  @Test
  public void insertTest() {
    productSystemParameterService.insert(productSystemParameter);
    Mockito.verify(productSystemParameterRepository).save(productSystemParameter);
  }

  @Test
  public void insertStoreIdBlankTest() {
    productSystemParameter.setStoreId(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productSystemParameterService.insert(productSystemParameter);
    });

  }

  @Test
  public void insertVariableBlankTest() {
    productSystemParameter.setVariable(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productSystemParameterService.insert(productSystemParameter);
    });

  }

  @Test
  public void insertDescriptionBlankTest() {
    productSystemParameter.setDescription(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productSystemParameterService.insert(productSystemParameter);
    });
  }

  @Test
  public void updateTest() {
    Mockito.when(productSystemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE_1))
        .thenReturn(productSystemParameter);
    productSystemParameterService.update(productSystemParameter1);
    Mockito.verify(productSystemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE_1);
    Mockito.verify(productSystemParameterRepository).save(productSystemParameter1);
  }

  @Test
  public void updateExceptionTest() {
    Mockito.when(productSystemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE_1))
        .thenReturn(null);
    try {

      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productSystemParameterService.update(productSystemParameter1);
      });
    } finally {
      Mockito.verify(productSystemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE_1);
    }
  }

  @Test
  public void deleteTest() {
    Mockito.when(productSystemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(productSystemParameter);
    productSystemParameterService.delete(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(productSystemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(productSystemParameterRepository).deleteById(ID);
  }

  @Test
  public void findByStoreIdAndVariableTest() {
    Mockito.when(productSystemParameterRepository.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE))
        .thenReturn(productSystemParameter);
    ProductSystemParameter response =
        productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Mockito.verify(productSystemParameterRepository).findByStoreIdAndVariable(DEFAULT_STORE_ID, VARIABLE);
    Assertions.assertEquals(VALUE, response.getValue());
    Assertions.assertEquals(DESCRIPTION, response.getDescription());
    Assertions.assertEquals(VARIABLE_1, response.getVariable());
  }

  @Test
  public void findByStoreIdAndShowOnUITrue() {
    productSystemParameter.setShowOnUI(true);
    List<ProductSystemParameter> productSystemParameterList = new ArrayList<>();
    productSystemParameterList.add(productSystemParameter);
    Mockito.when(productSystemParameterRepository.findByStoreIdAndShowOnUIIsTrue(DEFAULT_STORE_ID))
        .thenReturn(productSystemParameterList);
    List<ProductSystemParameter> response =
        productSystemParameterService.findByStoreIdAndShowOnUITrue(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterRepository).findByStoreIdAndShowOnUIIsTrue(DEFAULT_STORE_ID);
    Assertions.assertEquals(VALUE, response.get(0).getValue());
    Assertions.assertEquals(DESCRIPTION, response.get(0).getDescription());
    Assertions.assertEquals(VARIABLE_1, response.get(0).getVariable());
    Assertions.assertTrue(response.get(0).isShowOnUI());
  }

  @Test
  public void findSwitchValuesTest() {
    productSystemParameter = new ProductSystemParameter(VARIABLE_2, VALUE_2, DESCRIPTION, false);
    productSystemParameter1 = new ProductSystemParameter(VARIABLE_1, VALUE_3, DESCRIPTION, false);
    productSystemParameter2 = new ProductSystemParameter(VARIABLE_3, VALUE_2, DESCRIPTION, false);
    productSystemParameter3 = new ProductSystemParameter(VARIABLE_4, VALUE_3, DESCRIPTION, false);
    List<ProductSystemParameter> productSystemParameterList = new ArrayList<>();
    productSystemParameterList.add(productSystemParameter);
    productSystemParameterList.add(productSystemParameter1);
    productSystemParameterList.add(productSystemParameter2);
    productSystemParameterList.add(productSystemParameter3);
    Mockito.when(this.productSystemParameterRepository.findByStoreIdAndVariableIn(DEFAULT_STORE_ID, TEXT_VARIABLE_LIST))
        .thenReturn(productSystemParameterList);
    Map<String, Object> response = productSystemParameterService.findSwitchValues(DEFAULT_STORE_ID);
    Mockito.verify(this.productSystemParameterRepository)
        .findByStoreIdAndVariableIn(DEFAULT_STORE_ID, TEXT_VARIABLE_LIST);
    Mockito.verify(this.preOrderConfig).isPoQuotaFeatureSwitch();
    Assertions.assertTrue((Boolean) response.get(VARIABLE_2));
    Assertions.assertFalse((Boolean) response.get(VARIABLE_1));
    Assertions.assertEquals(new Long(10), response.get(SystemParameterConstants.MAX_STOCK_LIMIT));
    Assertions.assertTrue((boolean) response.get(SystemParameterConstants.PRE_ORDER_FEATURE_SWITCH_FOR_UI));
  }

  @Test
  public void findSwitchValuesCanarySwitchTest() {
    productSystemParameter = new ProductSystemParameter(VARIABLE_2, VALUE_2, DESCRIPTION, false);
    productSystemParameter1 = new ProductSystemParameter(VARIABLE_1, VALUE_3, DESCRIPTION, false);
    productSystemParameter2 = new ProductSystemParameter(VARIABLE_3, VALUE_2, DESCRIPTION, false);
    productSystemParameter3 = new ProductSystemParameter(VARIABLE_4, VALUE_3, DESCRIPTION, false);
    List<ProductSystemParameter> productSystemParameterList = new ArrayList<>();
    productSystemParameterList.add(productSystemParameter);
    productSystemParameterList.add(productSystemParameter1);
    productSystemParameterList.add(productSystemParameter2);
    productSystemParameterList.add(productSystemParameter3);
    Mockito.when(this.productSystemParameterRepository.findByStoreIdAndVariableIn(DEFAULT_STORE_ID, TEXT_VARIABLE_LIST))
        .thenReturn(productSystemParameterList);
    Map<String, Object> response = productSystemParameterService.findSwitchValues(DEFAULT_STORE_ID);
    Mockito.verify(this.productSystemParameterRepository)
        .findByStoreIdAndVariableIn(DEFAULT_STORE_ID, TEXT_VARIABLE_LIST);
    Mockito.verify(this.preOrderConfig).isPoQuotaFeatureSwitch();
    Assertions.assertTrue((Boolean) response.get(VARIABLE_2));
    Assertions.assertFalse((Boolean) response.get(VARIABLE_1));
    Assertions.assertEquals(new Long(10), response.get(SystemParameterConstants.MAX_STOCK_LIMIT));
  }

  @Test
  public void findSwitchValuesWithCanaryAndNonCanaryTest() {
    ReflectionTestUtils.setField(productSystemParameterService, "switchVariables", SWITCH_VARIALBLES);
    ReflectionTestUtils.setField(productSystemParameterService, "canaryFeatureSwitches",
        Map.of("sizeChartPhase1", "true"));
    productSystemParameter = new ProductSystemParameter(VARIABLE_2, VALUE_2, DESCRIPTION, false);
    productSystemParameter1 = new ProductSystemParameter(VARIABLE_1, VALUE_3, DESCRIPTION, false);
    productSystemParameter2 = new ProductSystemParameter(VARIABLE_3, VALUE_2, DESCRIPTION, false);
    productSystemParameter3 = new ProductSystemParameter(VARIABLE_4, VALUE_3, DESCRIPTION, false);
    List<ProductSystemParameter> productSystemParameterList = new ArrayList<>();
    productSystemParameterList.add(productSystemParameter);
    productSystemParameterList.add(productSystemParameter1);
    productSystemParameterList.add(productSystemParameter2);
    productSystemParameterList.add(productSystemParameter3);
    Mockito.when(applicationContext.getBean(ProductSystemParameterService.class))
        .thenReturn(productSystemParameterService);
    Mockito.when(productSystemParameterRepository.findByStoreIdAndVariableIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Map<String, Object> response =
        productSystemParameterService.findSwitchValuesWithCanaryAndNonCanary(DEFAULT_STORE_ID);
    Mockito.verify(applicationContext).getBean(ProductSystemParameterService.class);
    Mockito.verify(productSystemParameterRepository).findByStoreIdAndVariableIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.preOrderConfig).isPoQuotaFeatureSwitch();
    Assertions.assertEquals("true", response.get("sizeChartPhase1"));
  }
}