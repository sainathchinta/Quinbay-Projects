package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.util.Assert;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.repository.CatalogRepository;

public class CatalogServiceTest {

  private static final VerificationMode AT_LEAST_ONCE = Mockito.times(1);
  private static final String STORE_ID = "10001";
  private static final String ID = "123";
  private static final String CATALOG_CODE1 = "CATALOG_CODE_1";
  private static final String CATALOG_CODE2 = "CATALOG_CODE_2";
  private static final String CATALOG_CODE3 = "CATALOG_CODE_3";
  private static final String CATALOG_NAME1 = "CATALOG_NAME_1";
  private static final String CATALOG_NAME2 = "CATALOG_NAME_2";
  private static final String CATALOG_NAME3 = "CATALOG_NAME_3";
  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";

  @Mock
  private CatalogRepository repository;

  @InjectMocks
  private CatalogServiceBean service;


  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void postTest() {
    Mockito.verifyNoMoreInteractions(this.repository);
  }

  @Test
  public void testFindByCatalogType() {
    List<Catalog> masterCatalogs = new ArrayList<Catalog>();
    masterCatalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID));

    List<Catalog> salesCatalogs = new ArrayList<Catalog>();
    salesCatalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME2, CatalogServiceTest.CATALOG_CODE2,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));
    salesCatalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME3, CatalogServiceTest.CATALOG_CODE3,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));

    Page<Catalog> masterCatalogPage =
        new PageImpl<Catalog>(masterCatalogs, CatalogServiceTest.DEFAULT_PAGE_REQUEST, masterCatalogs.size());
    Page<Catalog> salesCatalogPage =
        new PageImpl<Catalog>(salesCatalogs, CatalogServiceTest.DEFAULT_PAGE_REQUEST, salesCatalogs.size());

    Mockito.when(this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogType.MASTER_CATALOG)).thenReturn(masterCatalogs);
    Mockito.when(this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogType.SALES_CATALOG)).thenReturn(salesCatalogs);

    Mockito.when(this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(masterCatalogPage);
    Mockito.when(this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogType.SALES_CATALOG, CatalogServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(salesCatalogPage);

    Assertions.assertTrue(
        this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.MASTER_CATALOG).size() == 1);
    Assertions
        .assertTrue(this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.SALES_CATALOG).size() == 2);

    Assertions.assertTrue(this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.MASTER_CATALOG)
        .size() == this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.MASTER_CATALOG,
            CatalogServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());
    Assertions.assertTrue(this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.SALES_CATALOG)
        .size() == this.service.findByCatalogType(CatalogServiceTest.STORE_ID, CatalogType.SALES_CATALOG,
            CatalogServiceTest.DEFAULT_PAGE_REQUEST).getTotalElements());

    Mockito.verify(this.repository, Mockito.times(2))
        .findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, CatalogType.MASTER_CATALOG);
    Mockito.verify(this.repository, Mockito.times(2))
        .findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, CatalogType.SALES_CATALOG);
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(
        CatalogServiceTest.STORE_ID, CatalogType.MASTER_CATALOG, CatalogServiceTest.DEFAULT_PAGE_REQUEST);
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(
        CatalogServiceTest.STORE_ID, CatalogType.SALES_CATALOG, CatalogServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindById() throws Exception {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    String id = GdnUUIDHelper.generateUUID();
    Mockito.when(this.repository.findById(id)).thenReturn(Optional.of(catalog));
    Catalog savedCatalog = this.service.findById(id);
    Assertions.assertEquals(savedCatalog, (catalog));
    Mockito.verify(this.repository, Mockito.times(1)).findById(id);
  }

  @Test
  public void testFindByName() {
    List<Catalog> catalogs = new ArrayList<Catalog>();
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID));
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME2, CatalogServiceTest.CATALOG_CODE2,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME3, CatalogServiceTest.CATALOG_CODE3,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));

    Page<Catalog> catalogPage =
        new PageImpl<Catalog>(catalogs, CatalogServiceTest.DEFAULT_PAGE_REQUEST, catalogs.size());

    String catalogNamePrefix = CatalogServiceTest.CATALOG_NAME1.split("_")[0];

    Mockito.when(this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        catalogNamePrefix)).thenReturn(catalogs);
    Mockito.when(this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        catalogNamePrefix, CatalogServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(catalogPage);

    Assertions.assertTrue(this.service.findByName(CatalogServiceTest.STORE_ID, catalogNamePrefix).size() == this.service
        .findByName(CatalogServiceTest.STORE_ID, catalogNamePrefix, CatalogServiceTest.DEFAULT_PAGE_REQUEST)
        .getTotalElements());

    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, catalogNamePrefix);
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE)
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, catalogNamePrefix,
            CatalogServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<Catalog> catalogs = new ArrayList<Catalog>();
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID));
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME2, CatalogServiceTest.CATALOG_CODE2,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));
    catalogs.add(new Catalog(CatalogServiceTest.CATALOG_NAME3, CatalogServiceTest.CATALOG_CODE3,
        CatalogType.SALES_CATALOG, CatalogServiceTest.STORE_ID));
    Page<Catalog> categoryPage =
        new PageImpl<Catalog>(catalogs, CatalogServiceTest.DEFAULT_PAGE_REQUEST, catalogs.size());
    Mockito.when(this.repository.findByStoreIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogServiceTest.DEFAULT_PAGE_REQUEST)).thenReturn(categoryPage);
    Assertions.assertTrue(this.service.findByStoreId(CatalogServiceTest.STORE_ID, CatalogServiceTest.DEFAULT_PAGE_REQUEST)
        .getTotalElements() == categoryPage.getTotalElements());
    Mockito.verify(this.repository, Mockito.times(1)).findByStoreIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID,
        CatalogServiceTest.DEFAULT_PAGE_REQUEST);
  }

  @Test
  public void testFindByStoreIdAndId() {
    String id = UUID.randomUUID().toString();
    Catalog savedCatalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    savedCatalog.setId(id);
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id))
        .thenReturn(savedCatalog);

    this.service.findByStoreIdAndId(CatalogServiceTest.STORE_ID, id);

    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndId() {
    String id = UUID.randomUUID().toString();
    Catalog savedCatalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    savedCatalog.setId(id);
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id))
        .thenReturn(savedCatalog);

    this.service.findDetailByStoreIdAndId(CatalogServiceTest.STORE_ID, id);

    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id);
  }

  @Test
  public void testFindDetailByStoreIdAndIdWithNullId() {
    String id = UUID.randomUUID().toString();
    try {
      this.service.findDetailByStoreIdAndId(CatalogServiceTest.STORE_ID, id);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      Assertions.assertTrue(e.getMessage().contains("not found catalog with id "));
      Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testMarkForDelete() throws Exception {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    String uuid = GdnUUIDHelper.generateUUID();
    catalog.setId(uuid);
    BeanUtils.copyProperties(catalog, savedCatalog);
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, catalog.getId()))
        .thenReturn(savedCatalog);
    Mockito.when(this.repository.findById(catalog.getId())).thenReturn(Optional.of(savedCatalog));

    this.service.markForDeleteCatalog(CatalogServiceTest.STORE_ID, catalog.getId());

    Assertions.assertTrue(savedCatalog.isMarkForDelete());
    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, catalog.getId());
    Mockito.verify(this.repository, Mockito.times(1)).findById(catalog.getId());
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).saveAndFlush(Mockito.any(Catalog.class));
  }

  @Test
  public void testMarkForDeleteWithEmptyId() throws Exception {
    String id = UUID.randomUUID().toString();
    try {
      this.service.markForDeleteCatalog(CatalogServiceTest.STORE_ID, id);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationException);
      Assertions.assertTrue(e.getMessage().contains("Can not perform delete on un exist data "));
      Mockito.verify(this.repository).findByStoreIdAndIdAndMarkForDeleteFalse(CatalogServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testSaveCatalogSuccessfully() throws Exception {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(catalog, savedCatalog);
    savedCatalog.setId(uuid);
    Mockito.when(this.repository.saveAndFlush(catalog)).thenReturn(savedCatalog);
    Assertions.assertEquals(this.service.save(catalog), (uuid));
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).saveAndFlush(catalog);
  }

  @Test
  public void testSaveCatalogWithEmptyId() {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    String uuid = GdnUUIDHelper.generateUUID();
    catalog.setId(uuid);
    BeanUtils.copyProperties(catalog, savedCatalog);
    Mockito.when(this.repository.findById(catalog.getId())).thenReturn(Optional.of(savedCatalog));
    try {
      this.service.save(catalog);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(CatalogServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
      Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).findById(catalog.getId());
    }

  }
  
	@Test
	public void testSaveCatalogWithEmptyCatalogCode() throws Exception {
		Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, "", CatalogType.MASTER_CATALOG,
				CatalogServiceTest.STORE_ID);
		Catalog savedCatalog = new Catalog();
		String uuid = GdnUUIDHelper.generateUUID();
		BeanUtils.copyProperties(catalog, savedCatalog);
		savedCatalog.setId(uuid);
		Mockito.when(this.repository.saveAndFlush(catalog)).thenReturn(savedCatalog);
		this.service.save(catalog);
		Mockito.verify(this.repository).getSequenceByCatalogCode("CA1");
		Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).saveAndFlush(catalog);
	}

  @Test
  public void testUpdateCatalogNonExistenceEntity() {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    String uuid = GdnUUIDHelper.generateUUID();
    catalog.setId(uuid);
    BeanUtils.copyProperties(catalog, savedCatalog);
    Mockito.when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(catalog);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(CatalogServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(catalog.getId());
    }
  }

  @Test
  public void testUpdateCatalogSuccessfully() throws Exception {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    String uuid = GdnUUIDHelper.generateUUID();
    catalog.setId(uuid);
    BeanUtils.copyProperties(catalog, savedCatalog);
    Mockito.when(this.repository.findById(catalog.getId())).thenReturn(Optional.of(savedCatalog));
    this.service.update(catalog);
    Assertions.assertTrue(true);
    Mockito.verify(this.repository, Mockito.times(1)).findById(catalog.getId());
    Mockito.verify(this.repository, CatalogServiceTest.AT_LEAST_ONCE).saveAndFlush(catalog);

  }

  @Test
  public void testUpdateCatalogWithEmptyId() {
    Catalog catalog = new Catalog(CatalogServiceTest.CATALOG_NAME1, CatalogServiceTest.CATALOG_CODE1,
        CatalogType.MASTER_CATALOG, CatalogServiceTest.STORE_ID);
    Catalog savedCatalog = new Catalog();
    BeanUtils.copyProperties(catalog, savedCatalog);
    try {
      this.service.update(catalog);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(CatalogServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
    }
  }
  
	@Test
	public void testgetSequence() throws Exception {
		long value = 234;
		Mockito.when(this.repository.getSequenceByCatalogCode(CatalogServiceTest.CATALOG_CODE1)).thenReturn(value);
		this.service.getSequence(CatalogServiceTest.CATALOG_CODE1);
		Mockito.verify(this.repository).getSequenceByCatalogCode(CatalogServiceTest.CATALOG_CODE1);
	}
	
	@Test
	public void testDelete() throws Exception {
		this.service.delete(CatalogServiceTest.ID);
	}

}
