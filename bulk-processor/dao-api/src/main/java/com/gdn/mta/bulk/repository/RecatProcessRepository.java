package com.gdn.mta.bulk.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.RecatProcess;

public interface RecatProcessRepository
    extends JpaRepository<RecatProcess, String>, RecatProcessCustomRepository {

  List<RecatProcess> findByStoreIdAndStatus(String storeId, String status);

  RecatProcess findByStoreIdAndRecatRequestCode(String storeId, String recatRequestCode);

}
