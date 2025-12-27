package com.gdn.aggregate.platform.module.product.listener.repository.semi;

import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.stream.Stream;

public interface SivaFlashsaleScheduleRepository extends MongoRepository<SivaFlashsaleSchedule, String> {

  Stream<SivaFlashsaleSchedule> streamAllByEndAfter(long timestamp);

}
