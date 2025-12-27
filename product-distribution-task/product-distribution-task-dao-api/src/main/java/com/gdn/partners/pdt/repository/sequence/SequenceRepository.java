package com.gdn.partners.pdt.repository.sequence;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.partners.pdt.entity.sequence.Sequence;

public interface SequenceRepository extends JpaRepository<Sequence, String> {

  String QUERY_GENERATE_BY_CODE = "SELECT generate_sequence(?1) AS sequence";

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query(value = SequenceRepository.QUERY_GENERATE_BY_CODE, nativeQuery = true)
  Long generateByCode(String code) throws Exception;

}
