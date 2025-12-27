package com.gdn.x.productcategorybase.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Catalog;

public interface CatalogService extends GdnBaseService<Catalog> {

  List<Catalog> findByCatalogType(String storeId, CatalogType catalogType);

  Page<Catalog> findByCatalogType(String storeId, CatalogType catalogType, Pageable pageable);

  List<Catalog> findByName(String storeId, String name);

  Page<Catalog> findByName(String storeId, String name, Pageable page);

  Catalog findByStoreIdAndId(String storeId, String id);

  Catalog findDetailByStoreIdAndId(String storeId, String id);

  String getSequence(String catalogCode);

  void markForDeleteCatalog(String storeId, String catalogId) throws Exception;
}
