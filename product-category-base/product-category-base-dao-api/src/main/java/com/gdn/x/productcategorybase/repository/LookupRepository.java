package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import com.gdn.x.productcategorybase.entity.Lookup;

public interface LookupRepository extends JpaRepository<Lookup, String>, JpaSpecificationExecutor<Lookup> {

  List<Lookup> findByLookupGroupAndMarkForDeleteFalse(String lookupGroup);
}
