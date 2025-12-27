package com.gdn.x.productcategorybase.service.categorytree;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;

import com.gdn.x.productcategorybase.repository.categorytree.CategoryTreeRepository;

public class CategoryTreeServiceTest {

  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private CategoryTreeRepository categoryTreeRepository;

  @InjectMocks
  private CategoryTreeServiceBean categoryTreeServiceBean;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    List<Object[]> categoryNodeRaws = this.generateCategoryNodeRaws();
    Mockito.when(
        this.categoryTreeRepository
            .findCategoryNodeByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(Mockito.anyString(),
                Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean())).thenReturn(categoryNodeRaws);
    Mockito.when(
        this.categoryTreeRepository.findCategoryRootNodeByStoreIdAndCatalogCodeAndActiveAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(categoryNodeRaws);
    Mockito.when(
        this.categoryTreeRepository
            .findCategoryNodeByStoreIdAndCatalogCodeAndParentCategoryCodeAndActiveAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(
        categoryNodeRaws);
    Mockito.when(
        this.categoryTreeRepository.findByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean())).thenReturn(
        categoryNodeRaws);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.categoryTreeRepository);
  }

  private Object[] generateCategoryNodeRaw() throws Exception {
    Object[] categoryNodeRaw = new Object[5];
    categoryNodeRaw[3] = false;
    categoryNodeRaw[4] = 0L;
    return categoryNodeRaw;
  }

  private List<Object[]> generateCategoryNodeRaws() throws Exception {
    List<Object[]> categoryNodeRaws = new ArrayList<Object[]>();
    categoryNodeRaws.add(this.generateCategoryNodeRaw());
    return categoryNodeRaws;
  }

  @SuppressWarnings("unchecked")
  @Test
  public void findCategoryNodeByCatalogCodeAndCategoryCodesAndActiveTest() throws Exception {
    this.categoryTreeServiceBean.findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(null, null, false);
    Mockito.verify(this.categoryTreeRepository)
        .findCategoryNodeByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActiveTest() throws Exception {
    this.categoryTreeServiceBean.findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(null,
        CategoryTreeServiceTest.DEFAULT_CATEGORY_CODE, false);
    Mockito.verify(this.categoryTreeRepository, CategoryTreeServiceTest.NEVER_CALLED)
        .findCategoryRootNodeByStoreIdAndCatalogCodeAndActiveAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(this.categoryTreeRepository)
        .findCategoryNodeByStoreIdAndCatalogCodeAndParentCategoryCodeAndActiveAndMarkForDeleteFalse(
            Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean());
  }

  @Test
  public void findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActiveWithParentCategoryCodeIsEmptyTest()
      throws Exception {
    this.categoryTreeServiceBean.findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(null, null, false);
    Mockito.verify(this.categoryTreeRepository)
        .findCategoryRootNodeByStoreIdAndCatalogCodeAndActiveAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.categoryTreeRepository, CategoryTreeServiceTest.NEVER_CALLED)
        .findCategoryNodeByStoreIdAndCatalogCodeAndParentCategoryCodeAndActiveAndMarkForDeleteFalse(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void findByCatalogCodeAndCategoryCodesAndActiveTest() throws Exception {
    this.categoryTreeServiceBean.findByCatalogCodeAndCategoryCodesAndActive(null, null, false);
    Mockito.verify(this.categoryTreeRepository)
        .findByStoreIdAndCatalogCodeAndCategoryCodesAndActiveAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.anyBoolean());
  }

}
