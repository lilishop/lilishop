package cn.lili.modules.member.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ClerkAddDTO;
import cn.lili.modules.member.entity.dto.ClerkEditDTO;
import cn.lili.modules.member.entity.dto.ClerkQueryDTO;
import cn.lili.modules.member.entity.vo.ClerkVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 店员业务层
 *
 * @author wget
 * @title: ClerkService
 * @projectName lilishop
 * @date 2021/12/28 7:42 下午
 */
public interface ClerkService extends IService<Clerk> {

    /**
     * 店员查询
     *
     * @param page
     * @param clerkQueryDTO
     * @return
     */
    IPage<ClerkVO> clerkForPage(PageVO page, ClerkQueryDTO clerkQueryDTO);

    /**
     * 查询店员详细
     *
     * @param id 店员id
     * @return
     */
    ClerkVO get(String id);

    /**
     * 修改店员信息
     *
     * @param clerkEditDTO 店员
     * @return
     */
    Clerk updateClerk(ClerkEditDTO clerkEditDTO);

    /**
     * 保存店员
     *
     * @param clerkAddDTO 店员
     * @return
     */
    Clerk saveClerk(ClerkAddDTO clerkAddDTO);

    /**
     * 根据会员id获取店员信息
     *
     * @param memberId 会员id
     * @return
     */
    Clerk getClerkByMemberId(String memberId);

    /**
     * 重置店员密码
     *
     * @param ids 店员ids
     */
    void resetPassword(List<String> ids);

    /**
     * 删除店员
     *
     * @param ids 店员ids
     */
    void deleteClerk(List<String> ids);

    /**
     * 检测会员有效性
     *
     * @param mobile 手机号码
     * @return
     */
    Member checkClerk(String mobile);

    /**
     * 店员状态操作
     *
     * @param id     店员id
     * @param status 状态
     */
    void disable(String id, Boolean status);


}
