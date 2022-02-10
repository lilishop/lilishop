package cn.lili.modules.page.serviceimpl;


import cn.lili.modules.page.entity.dos.Feedback;
import cn.lili.modules.page.mapper.FeedbackMapper;
import cn.lili.modules.page.service.FeedbackService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 意见反馈业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 11:40 上午
 */
@Service
public class FeedbackServiceImpl extends ServiceImpl<FeedbackMapper, Feedback> implements FeedbackService {

}