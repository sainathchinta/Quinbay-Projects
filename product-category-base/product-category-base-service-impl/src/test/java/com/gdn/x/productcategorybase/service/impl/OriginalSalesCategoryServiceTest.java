package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.repository.OriginalSalesCategoryRepository;
import com.gdn.x.productcategorybase.service.CategoryService;

public class OriginalSalesCategoryServiceTest {

  @InjectMocks
  private OriginalSalesCategoryServiceBean originalSalesCategoryService;

  @Mock
  private OriginalSalesCategoryRepository originalSalesCategoryRepository;

  @Mock
  private CategoryService categoryService;

  private static final String OSC_CODE = "oscCode";
  private static final String NEW_OSC_CODE = "newOscCode";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String OSC_TEXT = "oscLongText";
  private static final String KEYWORD = "keyword";
  private static final String ID = UUID.randomUUID().toString();
  private static final Boolean ACTIVATED = true;
  private List<OriginalSalesCategory> originalSalesCategories = new ArrayList<>();
  private OriginalSalesCategory originalSalesCategory;
  private OriginalSalesCategory newOriginalSalesCategory = new OriginalSalesCategory();
  private List<OscSummaryResponse> oscSummaryResponses;
  private OscInfoUpdateDTO oscInfoUpdateDTO;
  private List<Category> categories = new ArrayList<>();

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    originalSalesCategory = OriginalSalesCategory.builder().activated(ACTIVATED).oscCode(OSC_CODE).oscLongText(OSC_TEXT)
        .oscShortText(OSC_TEXT).build();
    categories.add(new Category(STORE_ID, CATEGORY_CODE, KEYWORD));
    originalSalesCategory.setMasterCategories(categories);
    originalSalesCategory.setId(ID);
    BeanUtils.copyProperties(originalSalesCategory, newOriginalSalesCategory);
    newOriginalSalesCategory.setOscCode(NEW_OSC_CODE);
    newOriginalSalesCategory.setActivated(true);
    originalSalesCategories.add(originalSalesCategory);
    oscInfoUpdateDTO =
        OscInfoUpdateDTO.builder().oscCode(OSC_CODE).oscShortText(OSC_TEXT).oscLongText(OSC_TEXT).activated(false)
            .newOscCode(NEW_OSC_CODE).build();
    Mockito.when(originalSalesCategoryRepository.findById(ID)).thenReturn(Optional.of(originalSalesCategory));
    Mockito.when(originalSalesCategoryRepository.findByOscCode(OSC_CODE)).thenReturn(originalSalesCategory);
    Mockito.when(originalSalesCategoryRepository.findByOscCode(NEW_OSC_CODE)).thenReturn(newOriginalSalesCategory);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.originalSalesCategoryRepository);
    Mockito.verifyNoMoreInteractions(this.categoryService);
  }

  @Test
  public void saveTest() throws Exception {
    Mockito.when(originalSalesCategoryRepository.saveAndFlush(originalSalesCategory)).thenReturn(originalSalesCategory);
    String id = this.originalSalesCategoryService.save(originalSalesCategory);
    Mockito.verify(originalSalesCategoryRepository).saveAndFlush(originalSalesCategory);
    Assertions.assertEquals(ID, id);
  }

  @Test
  public void filterSummaryOSCTest() {
    Mockito.when(originalSalesCategoryRepository.findByOscCodeAndNameAndActivated(OSC_CODE, KEYWORD, ACTIVATED))
        .thenReturn(originalSalesCategories);
    oscSummaryResponses = this.originalSalesCategoryService.filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED);
    Mockito.verify(originalSalesCategoryRepository).findByOscCodeAndNameAndActivated(OSC_CODE, KEYWORD, ACTIVATED);
    Assertions.assertEquals(ID, oscSummaryResponses.get(0).getId());
  }

  @Test
  public void filterSummaryOSC_NullTest() {
    Mockito.when(originalSalesCategoryRepository.findByOscCodeAndNameAndActivated(OSC_CODE, KEYWORD, ACTIVATED))
        .thenReturn(null);
    oscSummaryResponses = this.originalSalesCategoryService.filterSummaryOSC(OSC_CODE, KEYWORD, ACTIVATED);
    Mockito.verify(originalSalesCategoryRepository).findByOscCodeAndNameAndActivated(OSC_CODE, KEYWORD, ACTIVATED);
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrueTest() {
    Mockito.when(originalSalesCategoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(Constants.DEFAULT_STORE_ID, ID))
        .thenReturn(new OriginalSalesCategory());
    OriginalSalesCategory response = this.originalSalesCategoryService
        .findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(Constants.DEFAULT_STORE_ID, ID);
    Mockito.verify(originalSalesCategoryRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(Constants.DEFAULT_STORE_ID, ID);
    Assertions.assertNotNull(response);
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrueNullStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.originalSalesCategoryService.findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(null, ID));
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrueNullIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.originalSalesCategoryService
        .findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(Constants.DEFAULT_STORE_ID, null));
  }

  @Test
  public void updateOscTest_Deactivated() throws Exception {
    this.originalSalesCategoryService.updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
    Mockito.verify(originalSalesCategoryRepository, times(2)).findById(ID);
    Mockito.verify(originalSalesCategoryRepository, times(2)).findByOscCode(anyString());
    Mockito.verify(originalSalesCategoryRepository, times(2)).saveAndFlush(any(OriginalSalesCategory.class));
    Mockito.verify(categoryService).saveUpdatedCategory(any(), any(), any(),
        eq(Collections.singletonList(CategoryChangeEventType.CATEGORY_OSC_MAPPING_CHANGE)));
  }

  @Test
  public void updateOscTest_Deactivated_2() throws Exception {
    originalSalesCategory.setActivated(true);
    originalSalesCategory.setMasterCategories(null);
    Mockito.when(originalSalesCategoryRepository.findByOscCode(OSC_CODE)).thenReturn(originalSalesCategory);
    this.originalSalesCategoryService.updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
    Mockito.verify(originalSalesCategoryRepository, times(2)).findById(ID);
    Mockito.verify(originalSalesCategoryRepository, times(2)).findByOscCode(anyString());
    Mockito.verify(originalSalesCategoryRepository, times(2)).saveAndFlush(any(OriginalSalesCategory.class));
  }

  @Test
  public void updateOscTest_InactiveNewOsc() throws Exception {
    Mockito.when(originalSalesCategoryRepository.findByOscCode(NEW_OSC_CODE)).thenReturn(originalSalesCategory);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.originalSalesCategoryService.updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO));
    } finally {
      Mockito.verify(originalSalesCategoryRepository, times(2)).findByOscCode(anyString());
    }
  }

  @Test
  public void updateOscTest_NonDeactivation() throws Exception {
    oscInfoUpdateDTO.setActivated(true);
    this.originalSalesCategoryService.updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
    Mockito.verify(originalSalesCategoryRepository).findById(ID);
    Mockito.verify(originalSalesCategoryRepository).findByOscCode(OSC_CODE);
    Mockito.verify(originalSalesCategoryRepository).saveAndFlush(any(OriginalSalesCategory.class));
  }

  @Test
  public void updateOscTest_NewOscNull() throws Exception {
    oscInfoUpdateDTO.setNewOscCode(null);
    this.originalSalesCategoryService.updateOsc(STORE_ID, USERNAME, oscInfoUpdateDTO);
    Mockito.verify(originalSalesCategoryRepository).findById(ID);
    Mockito.verify(originalSalesCategoryRepository).findByOscCode(OSC_CODE);
    Mockito.verify(originalSalesCategoryRepository).saveAndFlush(any(OriginalSalesCategory.class));
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseTest() throws Exception {
    Mockito.when(originalSalesCategoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID))
        .thenReturn(originalSalesCategory);
    OriginalSalesCategoryResponse response =
        this.originalSalesCategoryService.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID);
    Mockito.verify(originalSalesCategoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID);
    Assertions.assertNotNull(response);
  }

  @Test
  public void findByStoreIdAndIdAndMarkForDeleteFalseWithNullResponseTest() throws Exception {
    Mockito.when(originalSalesCategoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID))
        .thenReturn(null);
    try {
      this.originalSalesCategoryService.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID);
    } catch (Exception e) {
    } finally {
      Mockito.verify(originalSalesCategoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, ID);
    }
  }
}
