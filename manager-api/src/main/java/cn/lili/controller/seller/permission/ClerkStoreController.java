package cn.lili.controller.seller.permission;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.validation.Phone;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ClerkAddDTO;
import cn.lili.modules.member.entity.dto.ClerkEditDTO;
import cn.lili.modules.member.entity.dto.ClerkQueryDTO;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.entity.vo.ClerkVO;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.ArrayList;
import java.util.List;


/**
 * 店员接口
 *
 * @author Chopper
 * @since 2020/11/16 10:57
 */
@Slf4j
@RestController
@Tag(name = "店员")
@RequestMapping("/store/clerk")
@Transactional(rollbackFor = Exception.class)
@Validated
public class ClerkStoreController {
    @Autowired
    private ClerkService clerkService;

    @Autowired
    private MemberService memberService;


    @GetMapping
    @Operation(description = "分页获取店员列表")
    @Parameter(name = "clerkQueryDTO", description = "店员查询参数", required = false)
    @Parameter(name = "pageVo", description = "分页参数", required = false)
    public ResultMessage<IPage<ClerkVO>> page(ClerkQueryDTO clerkQueryDTO,
                                              PageVO pageVo) {

        IPage<ClerkVO> page = clerkService.clerkForPage(pageVo, clerkQueryDTO);
        return ResultUtil.data(page);
    }

    @GetMapping("/{id}")
    @Operation(description = "获取店员详细")
    @Parameter(name = "id", description = "店员ID", required = true)
    public ResultMessage<ClerkVO> get(@PathVariable String id) {

        return ResultUtil.data(clerkService.get(id));
    }


    @PostMapping("/{mobile}/check")
    @Operation(description = "检测手机号码有效性")
    @Parameter(name = "mobile", description = "手机号码", required = true)
    public ResultMessage<Object> check(@PathVariable @Phone(message = "手机号码格式不正确") String mobile) {
        return ResultUtil.data(clerkService.checkClerk(mobile));
    }


    @PostMapping
    @Operation(description = "添加店员")
    @Parameter(name = "clerkAddDTO", description = "店员添加参数", required = true)
    public ResultMessage<Object> add(@Valid ClerkAddDTO clerkAddDTO) {
        int rolesMaxSize = 10;
        try {
            if (clerkAddDTO.getRoles() != null && clerkAddDTO.getRoles().size() >= rolesMaxSize) {
                throw new ServiceException(ResultCode.PERMISSION_BEYOND_TEN);
            }
            //校验是否已经是客户
            Member member = memberService.findByMobile(clerkAddDTO.getMobile());
            if (member == null) {
                //添加客户
                MemberAddDTO memberAddDTO = new MemberAddDTO();
                memberAddDTO.setMobile(clerkAddDTO.getMobile());
                memberAddDTO.setPassword(clerkAddDTO.getPassword());
                memberAddDTO.setUsername(clerkAddDTO.getUsername());
                member = memberService.addMember(memberAddDTO);
            } else {
                //校验要添加的客户是否已经是店主
                if (Boolean.TRUE.equals(member.getHaveStore())) {
                    throw new ServiceException(ResultCode.STORE_APPLY_DOUBLE_ERROR);
                }
                //校验客户的有效性
                if (Boolean.FALSE.equals(member.getDisabled())) {
                    throw new ServiceException(ResultCode.USER_STATUS_ERROR);
                }
            }
            //添加店员
            clerkAddDTO.setMemberId(member.getId());
            clerkAddDTO.setShopkeeper(false);
            clerkAddDTO.setStoreId(UserContext.getCurrentUser().getStoreId());
            clerkService.saveClerk(clerkAddDTO);
            //修改此客户拥有店铺
            List<String> ids = new ArrayList<>();
            ids.add(member.getId());
            memberService.updateHaveShop(true, UserContext.getCurrentUser().getStoreId(), ids);
        } catch (ServiceException se) {
            log.info(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error("添加店员出错", e);
        }
        return ResultUtil.success();
    }


    @PutMapping("/{id}")
    @Operation(description = "修改店员")
    @Parameter(name = "id", description = "店员id", required = true)
    @Parameter(name = "clerkEditDTO", description = "店员修改参数", required = true)
    public ResultMessage<Clerk> edit(@PathVariable String id, @Valid ClerkEditDTO clerkEditDTO) {
        clerkEditDTO.setId(id);
        return ResultUtil.data(clerkService.updateClerk(clerkEditDTO));
    }

    @PutMapping("/enable/{clerkId}")
    @Operation(description = "禁/启 用 店员")
    @Parameter(name = "clerkId", description = "店员ID", required = true)
    @Parameter(name = "status", description = "状态", required = true)
    @DemoSite
    public ResultMessage<Object> disable(@PathVariable String clerkId, Boolean status) {
        clerkService.disable(clerkId, status);
        return ResultUtil.success();
    }


    @DeleteMapping("/delByIds/{ids}")
    @Operation(description = "删除店员")
    @Parameter(name = "ids", description = "店员ID列表", required = true)
    public ResultMessage<Object> deleteClerk(@PathVariable List<String> ids) {
        clerkService.deleteClerk(ids);
        return ResultUtil.success();
    }


    @PostMapping("/resetPassword/{ids}")
    @Operation(description = "重置密码")
    @Parameter(name = "ids", description = "店员ID列表", required = true)
    @DemoSite
    public ResultMessage<Object> resetPassword(@PathVariable List<String> ids) {
        clerkService.resetPassword(ids);
        return ResultUtil.success(ResultCode.USER_EDIT_SUCCESS);
    }


}
