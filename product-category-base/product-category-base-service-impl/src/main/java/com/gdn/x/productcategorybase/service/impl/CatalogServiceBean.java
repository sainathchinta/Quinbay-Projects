package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.repository.CatalogRepository;
import com.gdn.x.productcategorybase.service.CatalogService;

@Service
@Transactional(readOnly = false)
public class CatalogServiceBean implements CatalogService {

  @Autowired
  private CatalogRepository repository;

  @Override
  @Transactional(readOnly = false)
  public void delete(String id) throws Exception {
    // TODO Auto-generated method stub
  }

  @Override
  public List<Catalog> findByCatalogType(String storeId, CatalogType catalogType) {
    return this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(storeId, catalogType);
  }

  @Override
  public Page<Catalog> findByCatalogType(String storeId, CatalogType catalogType, Pageable pageable) {
    return this.repository.findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(storeId, catalogType, pageable);
  }

  @Override
  public Catalog findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public List<Catalog> findByName(String storeId, String name) {
    return this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, name);
  }

  @Override
  public Page<Catalog> findByName(String storeId, String name, Pageable page) {
    return this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, name, page);
  }

  @Override
  public Page<Catalog> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public Catalog findByStoreIdAndId(String storeId, String id) {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  public Catalog findDetailByStoreIdAndId(String storeId, String id) {
    Catalog catalog = this.findByStoreIdAndId(storeId, id);
    GdnPreconditions.checkArgument(catalog != null, "not found catalog with id " + id);
    Hibernate.initialize(catalog.getCategories());
    return catalog;
  }

  @Override
  public String getSequence(String catalogCode) {
    return StringUtils.leftPad("" + this.repository.getSequenceByCatalogCode(catalogCode), 6, '0');
  }

  @Override
  @Transactional(readOnly = false)
  public void markForDeleteCatalog(String storeId, String catalogId) throws Exception {
    Catalog savedCatalog = this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, catalogId);
    if (savedCatalog == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Can not perform delete on un exist data : " + catalogId);
    }
    savedCatalog.setMarkForDelete(true);
    this.update(savedCatalog);
  }

  @Override
  @Transactional(readOnly = false)
  public String save(Catalog entity) throws Exception {
    if (StringUtils.isEmpty(entity.getCatalogCode())) {
      String prefix =
          StringUtils.left(entity.getName(), 2).toUpperCase() + StringUtils.right(entity.getName(), 1).toUpperCase();
      entity.setCatalogCode(prefix + "-0" + this.getSequence(prefix));
    }
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false)
  public void update(Catalog entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

}
