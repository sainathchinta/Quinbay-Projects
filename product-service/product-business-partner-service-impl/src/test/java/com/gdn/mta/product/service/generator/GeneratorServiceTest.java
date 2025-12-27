package com.gdn.mta.product.service.generator;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class GeneratorServiceTest {

  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";

  @Mock
  private CategoryRepository categoryRepository;

  @InjectMocks
  private GeneratorServiceBean generatorServiceBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    List<CategoryResponse> categoryResponses = this.generateCategoryResponses();
    Mockito.when(this.categoryRepository.findHierarchyByCategoryCode(Mockito.anyString()))
        .thenReturn(categoryResponses);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.categoryRepository);
  }

  private CatalogResponse generateCatalogResponse() throws Exception {
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    return catalogResponse;
  }

  private CategoryResponse generateCategoryResponse() throws Exception {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);
    categoryResponse.setLogisticAdjustment(100);
    categoryResponse.setCatalog(this.generateCatalogResponse());
    return categoryResponse;
  }

  private List<CategoryResponse> generateCategoryResponses() throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<CategoryResponse>();
    categoryResponses.add(this.generateCategoryResponse());
    return categoryResponses;
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    this.generatorServiceBean.generateShippingWeight(1D, 1D, 1D, 1D, GeneratorServiceTest.DEFAULT_CATEGORY_CODE);
    Mockito.verify(this.categoryRepository).findHierarchyByCategoryCode(Mockito.anyString());
  }

  @Test
  public void generateShippingWeightRoundOffToThreeDigitsTest() throws Exception {
    double response =
        this.generatorServiceBean.generateShippingWeight(1D, 1D, 1D, 0.001, GeneratorServiceTest.DEFAULT_CATEGORY_CODE);
    Mockito.verify(this.categoryRepository).findHierarchyByCategoryCode(Mockito.anyString());
    Assertions.assertEquals(0.001, response, 0);
  }

  @Test
  public void generateShippingWeightWithCategoryNotExistExceptionTest() throws Exception {
    Mockito.when(this.categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.generatorServiceBean.generateShippingWeight(1D, 1D, 1D, 1D, GeneratorServiceTest.DEFAULT_CATEGORY_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_NOT_FOUND, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.categoryRepository).findHierarchyByCategoryCode(Mockito.anyString());

  }

  @Test
  public void generateShippingWeightWithCategoryEmptyExceptionTest() throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<CategoryResponse>();
    Mockito.when(this.categoryRepository.findHierarchyByCategoryCode(Mockito.anyString()))
        .thenReturn(categoryResponses);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.generatorServiceBean.generateShippingWeight(1D, 1D, 1D, 1D, GeneratorServiceTest.DEFAULT_CATEGORY_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_NOT_FOUND, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.categoryRepository).findHierarchyByCategoryCode(Mockito.anyString());

  }

  @Test
  public void generateShippingWeightWithCatalogTypeInvalidExceptionTest() throws Exception {
    CategoryResponse categoryResponse = this.generateCategoryResponse();
    categoryResponse.getCatalog().setCatalogType(CatalogType.SALES_CATALOG.name());
    List<CategoryResponse> categoryResponses = new ArrayList<CategoryResponse>();
    categoryResponses.add(categoryResponse);
    Mockito.when(this.categoryRepository.findHierarchyByCategoryCode(Mockito.anyString()))
        .thenReturn(categoryResponses);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.generatorServiceBean.generateShippingWeight(1D, 1D, 1D, 1D, GeneratorServiceTest.DEFAULT_CATEGORY_CODE);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.VALIDATION, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.categoryRepository).findHierarchyByCategoryCode(Mockito.anyString());

  }

}
