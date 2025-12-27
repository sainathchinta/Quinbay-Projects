package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.mta.product.util.BeanUtils;

import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductDataAutoFixHistory;
import com.gdn.mta.product.repository.ProductDataAutoFixHistoryRepository;

public class ProductDataAutoFixHistoryServiceImplTest {

  private static final String PRODUCT_CODE = "ABC123";
  private static final String ADDITIONAL_INFO = "product Images was missisng";
  private static final String TYPE = "AUTO_HEAL";

  private ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto;
  private ProductDataAutoFixHistoryDto productDataAutoFixHistoryDtoEmpty;
  private ProductDataAutoFixHistory productDataAutoFixHistory;

  List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList;
  List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoListEmpty;
  List<ProductDataAutoFixHistory> productDataAutoFixHistoryList;

  @InjectMocks
  private ProductDataAutoFixHistoryServiceImpl productDataAutoFixHistoryServiceImpl;

  @Mock
  private ProductDataAutoFixHistoryRepository productDataAutoFixHistoryRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistoryDtoEmpty = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistory = new ProductDataAutoFixHistory();
    productDataAutoFixHistoryDtoList = new ArrayList<>();
    productDataAutoFixHistoryDtoListEmpty = new ArrayList<>();
    productDataAutoFixHistoryList = new ArrayList<>();
    productDataAutoFixHistoryDto.setProductCode(PRODUCT_CODE);
    productDataAutoFixHistoryDto.setType(TYPE);
    productDataAutoFixHistoryDto.setAdditionalInfo(ADDITIONAL_INFO);
    productDataAutoFixHistoryDtoEmpty.setProductCode(PRODUCT_CODE);
    productDataAutoFixHistoryDtoEmpty.setType(StringUtils.EMPTY);
    productDataAutoFixHistoryDtoEmpty.setAdditionalInfo(ADDITIONAL_INFO);
    productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
    productDataAutoFixHistoryDtoListEmpty.add(productDataAutoFixHistoryDtoEmpty);
    BeanUtils.copyProperties(productDataAutoFixHistoryDto, productDataAutoFixHistory);
    productDataAutoFixHistoryList.add(productDataAutoFixHistory);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productDataAutoFixHistoryRepository);
  }

  @Test
  public void saveHistoryTest() throws ApplicationException {
    this.productDataAutoFixHistoryServiceImpl.saveHistory(productDataAutoFixHistoryDtoList);
    Mockito.verify(productDataAutoFixHistoryRepository, Mockito.times(1)).saveAll(productDataAutoFixHistoryList);
  }

  @Test
  public void saveHistoryNullTest() throws ApplicationException {
    Assertions.assertThrows(ApplicationException.class, () -> {
      this.productDataAutoFixHistoryServiceImpl.saveHistory(null);
    });
  }
  @Test
  public void saveHistoryEmptyTypeTest() throws ApplicationRuntimeException, ApplicationException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productDataAutoFixHistoryServiceImpl.saveHistory(productDataAutoFixHistoryDtoListEmpty);
    });
  }
}