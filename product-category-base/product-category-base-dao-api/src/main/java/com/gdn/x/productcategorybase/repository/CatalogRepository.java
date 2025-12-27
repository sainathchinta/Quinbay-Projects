package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.entity.Catalog;

public interface CatalogRepository extends JpaRepository<Catalog, String> {

  List<Catalog> findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(String storeId, CatalogType catalogType);

  Page<Catalog> findByStoreIdAndCatalogTypeAndMarkForDeleteFalse(String storeId, CatalogType catalogType,
      Pageable pageable);

  Catalog findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String catalogId);

  Page<Catalog> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  @Query(value = "SELECT c FROM Catalog c WHERE c.storeId = :storeId AND c.name LIKE :name% AND c.markForDelete = false")
  List<Catalog> findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(@Param("storeId")String storeId, @Param("name") String name);

  @Query(value = "SELECT c FROM Catalog c WHERE c.storeId = :storeId AND c.name LIKE :name% AND c.markForDelete = false")
  Page<Catalog> findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(@Param("storeId")String storeId,
      @Param("name") String name, Pageable pageable);

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByCatalogCode(String catalogCode);
}
